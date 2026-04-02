#!/usr/bin/env bash
set -euo pipefail

ENV_NAME="${ENV_NAME:-bart-env}"
CONFIGFILE="${CONFIGFILE:-config/snakemake_pipeline.yaml}"
JOBS="${JOBS:-21}"
TARGETS=("$@")
RUN_TAG="${RUN_TAG:-$(date +%Y%m%d_%H%M%S)}"
PIPELINE_ROOT="${PIPELINE_ROOT:-output/pipeline_snakemake_${RUN_TAG}}"

if ! conda run -n "$ENV_NAME" python -c 'import snakemake' >/dev/null 2>&1; then
  echo "snakemake is not installed in Conda env '$ENV_NAME'. Install it there first." >&2
  exit 1
fi

mkdir -p logs

cmd=(
  conda run -n "$ENV_NAME" snakemake
  --snakefile Snakefile
  --configfile "$CONFIGFILE"
  --config "run_tag=$RUN_TAG" "pipeline_root=$PIPELINE_ROOT"
  --jobs "$JOBS"
  --rerun-incomplete
  --keep-going
  --latency-wait 60
  --printshellcmds
  --cluster "bsub -J {rule}.{wildcards} -q {resources.queue} -n {threads} -M {resources.mem_mb} -R span[hosts=1] -R rusage[mem={resources.mem_rusage_mb}] -W {resources.runtime_min} -oo logs/smk_{rule}.{wildcards}.%J.out -eo logs/smk_{rule}.{wildcards}.%J.err"
)

if [[ ${#TARGETS[@]} -gt 0 ]]; then
  cmd+=(--)
  cmd+=("${TARGETS[@]}")
fi

echo "RUN_TAG=$RUN_TAG"
echo "PIPELINE_ROOT=$PIPELINE_ROOT"
"${cmd[@]}"
