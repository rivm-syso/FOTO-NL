import math
import time

configfile: 'config/snakemake_pipeline.yaml'
shell.executable('/bin/bash')

def as_bool(value, default=False):
    if value is None:
        return default
    if isinstance(value, bool):
        return value
    if isinstance(value, (int, float)):
        return bool(value)
    return str(value).strip().lower() in ('1', 'true', 'yes', 'y', 'on')

BOARD_MAP = {
    'aa_en_maas': 'Aa en Maas',
    'amstel_gooi_vecht': 'Amstel Gooi Vecht',
    'brabantse_delta': 'Brabantse Delta',
    'de_dommel': 'De Dommel',
    'delfland': 'Delfland',
    'drents_overijsselse_delta': 'Drents Overijsselse Delta',
    'hh_van_rijnland': 'HH van Rijnland',
    'hollands_noorderkwartier': 'Hollands Noorderkwartier',
    'hollandse_delta': 'Hollandse Delta',
    'hunze_en_aas': 'Hunze en Aas',
    'limburg': 'Limburg',
    'noorderzijlvest': 'Noorderzijlvest',
    'rijn_en_ijssel': 'Rijn en IJssel',
    'rivierenland': 'Rivierenland',
    'scheldestromen': 'Scheldestromen',
    'schieland_krimpenerwaard': 'Schieland Krimpenerwaard',
    'stichtse_rijnlanden': 'Stichtse Rijnlanden',
    'vallei_veluwe': 'Vallei Veluwe',
    'vechtstromen': 'Vechtstromen',
    'wetterskip': 'Wetterskip',
    'zuiderzeeland': 'Zuiderzeeland',
}
BOARD_SLUGS = list(BOARD_MAP.keys())
REPORT_BOARD_CONFIG = config.get('script_0_report_boards', [])
INCLUDE_SCRIPT_0_REPORTS = as_bool(config.get('include_script_0_reports', False))
INCLUDE_RAW_BRANCH = as_bool(config.get('include_raw_branch', True), default=True)
INCLUDE_COMPARISON = as_bool(config.get('include_comparison', False))
INCLUDE_QA = as_bool(config.get('include_qa', False))
if INCLUDE_QA:
    INCLUDE_COMPARISON = True
if REPORT_BOARD_CONFIG in ('all', 'ALL', '*'):
    REPORT_BOARD_SLUGS = BOARD_SLUGS
elif isinstance(REPORT_BOARD_CONFIG, list) and any(str(x).lower() in ('all', '*') for x in REPORT_BOARD_CONFIG):
    REPORT_BOARD_SLUGS = BOARD_SLUGS
else:
    REPORT_BOARD_SLUGS = [b for b in REPORT_BOARD_CONFIG if b in BOARD_MAP]
if not INCLUDE_SCRIPT_0_REPORTS:
    REPORT_BOARD_SLUGS = []
RUN_TAG = config.get('run_tag') or time.strftime('%Y%m%d_%H%M%S')
PIPELINE_ROOT = config.get('pipeline_root') or f'output/pipeline_snakemake_{RUN_TAG}'
RAW_ROOT = config.get('raw_root', 'Chemie data waterschappen')
REFERENCE_INPUT_ROOT = config.get('reference_input_root', 'auxiliary_workflow_input')
BASELINE_CLEAN_CSV = config.get('baseline_clean_csv', 'data/zenodo_14652471/FOTO_NL_output_clean.csv')
ENV_NAME = config.get('env_name', 'bart-env')
QUEUE = config.get('queue', 'bio')
QA = config.get('qa', {})

SCRIPT_0_ROOT = f'{PIPELINE_ROOT}/script_0_output'
WORKSPACE_CLEAN = f'{PIPELINE_ROOT}/workspace_clean'
WORKSPACE_RAW = f'{PIPELINE_ROOT}/workspace_raw'
COMPARISON_ROOT = f'{PIPELINE_ROOT}/comparison_clean_vs_original'
QA_ROOT = f'{PIPELINE_ROOT}/qa'
SCRIPT_0_REPORT_ROOT = f'{PIPELINE_ROOT}/script_0_reports'
SCRIPT_0_WRITE_DIAGNOSTICS = as_bool(config.get('script_0_write_diagnostics', False))
SCRIPT_0_WRITE_DIAGNOSTICS_ENV = 'true' if SCRIPT_0_WRITE_DIAGNOSTICS else 'false'
SCRIPT_0_CSV_PATTERN = f'{SCRIPT_0_ROOT}/{{board_slug}}/board_input.csv'
SCRIPT_0_REPORT_PATTERN = f'{SCRIPT_0_REPORT_ROOT}/{{board_slug}}_script_0_report.md'

localrules: all

rule all:
    input:
        ([f'{WORKSPACE_CLEAN}/script_3_summary_statistics.html'] +
         ([f'{WORKSPACE_RAW}/script_3_summary_statistics.html'] if INCLUDE_RAW_BRANCH else []) +
         ([f'{COMPARISON_ROOT}/clean_vs_original_by_board_substance.csv'] if INCLUDE_COMPARISON else []) +
         ([f'{QA_ROOT}/clean_vs_original.pass'] if INCLUDE_QA else []) +
         expand(SCRIPT_0_REPORT_PATTERN, board_slug=REPORT_BOARD_SLUGS))

rule script_0_board:
    input:
        script='script_0_raw_input_processing.R',
        common='helpers/script_0/build_input_common.R',
        cfg='helpers/script_0/build_input_board_configs.R'
    output:
        csv=SCRIPT_0_CSV_PATTERN
    params:
        board_name=lambda wc: BOARD_MAP[wc.board_slug],
        output_root=lambda wc: f'{SCRIPT_0_ROOT}/{wc.board_slug}'
    threads: 1
    resources:
        queue=QUEUE,
        mem_mb=int(config.get('script_0_mem_mb', 32768)),
        mem_rusage_mb=int(config.get('script_0_mem_mb', 32768)),
        runtime_min=int(config.get('script_0_runtime_min', 720))
    shell:
        r'''
        mkdir -p logs {params.output_root}
        RUN_0_RAW_INPUT_PROCESSING=true \
        RAW_ROOT='{RAW_ROOT}' \
        REFERENCE_INPUT_ROOT='{REFERENCE_INPUT_ROOT}' \
        ONLY_SOURCES='{params.board_name}' \
        OUTPUT_ROOT='{params.output_root}' \
        SCRIPT_0_WRITE_DIAGNOSTICS='{SCRIPT_0_WRITE_DIAGNOSTICS_ENV}' \
        OUTPUT_FORMAT='csv' \
        OVERWRITE='true' \
        INCLUDE_LEGACY_INPUTS='false' \
        conda run -n '{ENV_NAME}' Rscript script_0_raw_input_processing.R
        '''

rule script_0_report:
    input:
        csv=SCRIPT_0_CSV_PATTERN,
        runner='script_0_report.R',
        report='script_0_report.Rmd',
        common='helpers/script_0/build_input_common.R',
        cfg='helpers/script_0/build_input_board_configs.R'
    output:
        report=SCRIPT_0_REPORT_PATTERN
    params:
        board_name=lambda wc: BOARD_MAP[wc.board_slug],
        output_dir=SCRIPT_0_REPORT_ROOT,
        report_preview_n=int(config.get('script_0_report_preview_n', 8)),
        max_rows_per_file=config.get('script_0_report_max_rows_per_file', '')
    threads: 1
    resources:
        queue=QUEUE,
        mem_mb=int(config.get('script_0_report_mem_mb', 8192)),
        mem_rusage_mb=int(config.get('script_0_report_mem_mb', 8192)),
        runtime_min=int(config.get('script_0_report_runtime_min', 240))
    shell:
        r'''
        mkdir -p logs '{params.output_dir}' '{SCRIPT_0_ROOT}/{wildcards.board_slug}'
        RAW_ROOT='{RAW_ROOT}' \
        REFERENCE_INPUT_ROOT='{REFERENCE_INPUT_ROOT}' \
        OUTPUT_ROOT='{SCRIPT_0_ROOT}/{wildcards.board_slug}' \
        SCRIPT_0_WRITE_DIAGNOSTICS='true' \
        DRY_RUN='true' \
        SCRIPT_0_COLLECT_DEBUG='true' \
        SCRIPT_0_REPORT_PREVIEW_N='{params.report_preview_n}' \
        MAX_ROWS_PER_FILE='{params.max_rows_per_file}' \
        conda run -n '{ENV_NAME}' Rscript script_0_report.R \
          --board='{params.board_name}' \
          --report-format=md \
          --output-dir='{params.output_dir}'
        '''

rule script1_clean:
    input:
        boards=expand(SCRIPT_0_CSV_PATTERN, board_slug=BOARD_SLUGS),
        notebook='script_1_read_and_clean.Rmd'
    output:
        html=f'{WORKSPACE_CLEAN}/script_1_read_and_clean.html',
        topaf_rds=f'{WORKSPACE_CLEAN}/ToPAF.rds',
        topaf_rda=f'{WORKSPACE_CLEAN}/ToPAF.rda',
        all_data=f'{WORKSPACE_CLEAN}/AllData.RDS',
        inb=f'{WORKSPACE_CLEAN}/InBAllData.RDS'
    threads: int(config.get('script1_threads', 16))
    resources:
        queue=QUEUE,
        mem_mb=int(config.get('script1_mem_mb', 131072)),
        mem_rusage_mb=math.ceil(int(config.get('script1_mem_mb', 131072)) / max(int(config.get('script1_threads', 16)), 1)),
        runtime_min=int(config.get('script1_runtime_min', 720))
    shell:
        r'''
        rm -rf '{WORKSPACE_CLEAN}'
        mkdir -p '{WORKSPACE_CLEAN}'
        ln -sfn "$(realpath script_1_read_and_clean.Rmd)" '{WORKSPACE_CLEAN}/script_1_read_and_clean.Rmd'
        ln -sfn "$(realpath script_2_spatial_analysis.Rmd)" '{WORKSPACE_CLEAN}/script_2_spatial_analysis.Rmd'
        ln -sfn "$(realpath script_3_summary_statistics.Rmd)" '{WORKSPACE_CLEAN}/script_3_summary_statistics.Rmd'
        ln -sfn "$(realpath helpers)" '{WORKSPACE_CLEAN}/helpers'
        ln -sfn "$(realpath resources)" '{WORKSPACE_CLEAN}/resources'
        if [ -d data ]; then
          ln -sfn "$(realpath data)" '{WORKSPACE_CLEAN}/data'
        else
          mkdir -p '{WORKSPACE_CLEAN}/data'
        fi
        if [ -d '{REFERENCE_INPUT_ROOT}' ]; then
          ln -sfn "$(realpath '{REFERENCE_INPUT_ROOT}')" '{WORKSPACE_CLEAN}/auxiliary_input'
        fi
        cd '{WORKSPACE_CLEAN}'
        OMP_NUM_THREADS='{threads}' \
        OPENBLAS_NUM_THREADS='{threads}' \
        MKL_NUM_THREADS='{threads}' \
        NUMEXPR_NUM_THREADS='{threads}' \
        STAGE1_N_CORES='{threads}' \
        PRJDATA_DIR="$(realpath ../script_0_output)" \
        LIGHTWEIGHT_CHECKS='true' \
        STRICT_SCRIPT_0_DIAG='false' \
        conda run -n '{ENV_NAME}' Rscript -e "rmarkdown::render('script_1_read_and_clean.Rmd', quiet=FALSE)"
        '''

rule prepare_workspace_raw:
    input:
        all_data=f'{WORKSPACE_CLEAN}/AllData.RDS'
    output:
        touch(f'{WORKSPACE_RAW}/.prepared')
    threads: 1
    resources:
        queue=QUEUE,
        mem_mb=2048,
        mem_rusage_mb=2048,
        runtime_min=60
    shell:
        r'''
        rm -rf '{WORKSPACE_RAW}'
        mkdir -p '{WORKSPACE_RAW}'
        ln -sfn "$(realpath script_2_spatial_analysis_raw.Rmd)" '{WORKSPACE_RAW}/script_2_spatial_analysis_raw.Rmd'
        ln -sfn "$(realpath script_3_summary_statistics.Rmd)" '{WORKSPACE_RAW}/script_3_summary_statistics.Rmd'
        ln -sfn "$(realpath helpers)" '{WORKSPACE_RAW}/helpers'
        ln -sfn "$(realpath resources)" '{WORKSPACE_RAW}/resources'
        if [ -d data ]; then
          ln -sfn "$(realpath data)" '{WORKSPACE_RAW}/data'
        else
          mkdir -p '{WORKSPACE_RAW}/data'
        fi
        if [ -d '{REFERENCE_INPUT_ROOT}' ]; then
          ln -sfn "$(realpath '{REFERENCE_INPUT_ROOT}')" '{WORKSPACE_RAW}/auxiliary_input'
        fi
        ln -sfn "$(realpath {input.all_data})" '{WORKSPACE_RAW}/AllData.RDS'
        touch {output}
        '''

rule script2_clean:
    input:
        topaf=f'{WORKSPACE_CLEAN}/ToPAF.rds',
        notebook='script_2_spatial_analysis.Rmd'
    output:
        html=f'{WORKSPACE_CLEAN}/script_2_spatial_analysis.html',
        all_data_nl=f'{WORKSPACE_CLEAN}/AllDataNL.rds',
        csv=f'{WORKSPACE_CLEAN}/output/FOTO_NL_output_clean.csv'
    threads: int(config.get('script2_threads', 12))
    resources:
        queue=QUEUE,
        mem_mb=int(config.get('script2_mem_mb', 98304)),
        mem_rusage_mb=math.ceil(int(config.get('script2_mem_mb', 98304)) / max(int(config.get('script2_threads', 12)), 1)),
        runtime_min=int(config.get('script2_runtime_min', 480))
    shell:
        r'''
        cd '{WORKSPACE_CLEAN}'
        OMP_NUM_THREADS='{threads}' \
        OPENBLAS_NUM_THREADS='{threads}' \
        MKL_NUM_THREADS='{threads}' \
        NUMEXPR_NUM_THREADS='{threads}' \
        conda run -n '{ENV_NAME}' Rscript -e "rmarkdown::render('script_2_spatial_analysis.Rmd', quiet=FALSE)"
        '''

rule script2_raw:
    input:
        prep=f'{WORKSPACE_RAW}/.prepared',
        notebook='script_2_spatial_analysis_raw.Rmd'
    output:
        html=f'{WORKSPACE_RAW}/script_2_spatial_analysis_raw.html',
        all_data_nl=f'{WORKSPACE_RAW}/AllDataNL.rds',
        csv=f'{WORKSPACE_RAW}/output/FOTO_NL_output_raw.csv'
    threads: int(config.get('script2_raw_threads', 12))
    resources:
        queue=QUEUE,
        mem_mb=int(config.get('script2_raw_mem_mb', 98304)),
        mem_rusage_mb=math.ceil(int(config.get('script2_raw_mem_mb', 98304)) / max(int(config.get('script2_raw_threads', 12)), 1)),
        runtime_min=int(config.get('script2_raw_runtime_min', 480))
    shell:
        r'''
        cd '{WORKSPACE_RAW}'
        OMP_NUM_THREADS='{threads}' \
        OPENBLAS_NUM_THREADS='{threads}' \
        MKL_NUM_THREADS='{threads}' \
        NUMEXPR_NUM_THREADS='{threads}' \
        conda run -n '{ENV_NAME}' Rscript -e "rmarkdown::render('script_2_spatial_analysis_raw.Rmd', quiet=FALSE)"
        '''

rule script3_clean:
    input:
        all_data_nl=f'{WORKSPACE_CLEAN}/AllDataNL.rds',
        notebook='script_3_summary_statistics.Rmd'
    output:
        html=f'{WORKSPACE_CLEAN}/script_3_summary_statistics.html'
    threads: int(config.get('script3_threads', 8))
    resources:
        queue=QUEUE,
        mem_mb=int(config.get('script3_mem_mb', 32768)),
        mem_rusage_mb=math.ceil(int(config.get('script3_mem_mb', 32768)) / max(int(config.get('script3_threads', 8)), 1)),
        runtime_min=int(config.get('script3_runtime_min', 480))
    shell:
        r'''
        cd '{WORKSPACE_CLEAN}'
        OMP_NUM_THREADS='{threads}' \
        OPENBLAS_NUM_THREADS='{threads}' \
        MKL_NUM_THREADS='{threads}' \
        NUMEXPR_NUM_THREADS='{threads}' \
        conda run -n '{ENV_NAME}' Rscript -e "rmarkdown::render('script_3_summary_statistics.Rmd', quiet=FALSE)"
        '''

rule script3_raw:
    input:
        all_data_nl=f'{WORKSPACE_RAW}/AllDataNL.rds',
        notebook='script_3_summary_statistics.Rmd'
    output:
        html=f'{WORKSPACE_RAW}/script_3_summary_statistics.html'
    threads: int(config.get('script3_threads', 8))
    resources:
        queue=QUEUE,
        mem_mb=int(config.get('script3_mem_mb', 32768)),
        mem_rusage_mb=math.ceil(int(config.get('script3_mem_mb', 32768)) / max(int(config.get('script3_threads', 8)), 1)),
        runtime_min=int(config.get('script3_runtime_min', 480))
    shell:
        r'''
        cd '{WORKSPACE_RAW}'
        OMP_NUM_THREADS='{threads}' \
        OPENBLAS_NUM_THREADS='{threads}' \
        MKL_NUM_THREADS='{threads}' \
        NUMEXPR_NUM_THREADS='{threads}' \
        conda run -n '{ENV_NAME}' Rscript -e "rmarkdown::render('script_3_summary_statistics.Rmd', quiet=FALSE)"
        '''

rule compare_clean:
    input:
        script='helpers/python/compare_clean_outputs.py',
        orig=BASELINE_CLEAN_CSV,
        new=f'{WORKSPACE_CLEAN}/output/FOTO_NL_output_clean.csv'
    output:
        detail=f'{COMPARISON_ROOT}/clean_vs_original_by_board_substance.csv',
        summary=f'{COMPARISON_ROOT}/clean_vs_original_board_summary.csv'
    log:
        f'{COMPARISON_ROOT}/compare_clean.log'
    threads: 1
    resources:
        queue=QUEUE,
        mem_mb=4096,
        mem_rusage_mb=4096,
        runtime_min=120
    shell:
        r'''
        mkdir -p '{COMPARISON_ROOT}'
        python {input.script} --orig '{input.orig}' --new '{input.new}' --outdir '{COMPARISON_ROOT}' > '{log}' 2>&1
        '''

rule qa_drift:
    input:
        script='helpers/python/qa_compare_clean_outputs.py',
        detail=f'{COMPARISON_ROOT}/clean_vs_original_by_board_substance.csv',
        summary=f'{COMPARISON_ROOT}/clean_vs_original_board_summary.csv'
    output:
        f'{QA_ROOT}/clean_vs_original.pass'
    log:
        f'{QA_ROOT}/clean_vs_original.log'
    params:
        max_orig_only_keys=int(QA.get('max_orig_only_keys', 200)),
        max_new_only_keys=int(QA.get('max_new_only_keys', 200)),
        max_unit_mismatch_common=int(QA.get('max_unit_mismatch_common', 0)),
        max_negative_new_mean_common=int(QA.get('max_negative_new_mean_common', 0)),
        max_abs_mean_rel_diff_over_threshold=int(QA.get('max_abs_mean_rel_diff_over_threshold', 0)),
        mean_rel_diff_threshold=float(QA.get('mean_rel_diff_threshold', 10.0))
    threads: 1
    resources:
        queue=QUEUE,
        mem_mb=2048,
        mem_rusage_mb=2048,
        runtime_min=60
    shell:
        r'''
        mkdir -p '{QA_ROOT}'
        python {input.script} \
          --detail '{input.detail}' \
          --summary '{input.summary}' \
          --pass-file '{output}' \
          --max-orig-only-keys {params.max_orig_only_keys} \
          --max-new-only-keys {params.max_new_only_keys} \
          --max-unit-mismatch-common {params.max_unit_mismatch_common} \
          --max-negative-new-mean-common {params.max_negative_new_mean_common} \
          --max-abs-mean-rel-diff-over-threshold {params.max_abs_mean_rel_diff_over_threshold} \
          --mean-rel-diff-threshold {params.mean_rel_diff_threshold} \
          > '{log}' 2>&1
        '''
