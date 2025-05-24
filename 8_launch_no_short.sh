#!/usr/bin/env bash
###############################################################################
#  Run Portfolio-ML jobs  — one per gamma, choose smoke | mini | prod
###############################################################################
set -euo pipefail

CPU_LIST="6-31"                     # pin to your free logical cores
GAMMA_LIST=(1 5 10 20 100)          # frontier grid
MODE=${1:-mini}                     # default = mini
MEM_LIMIT=${2:-8G}                  # memory limit (default: 8GB)
WEALTH=${3:-1e10}                   # wealth parameter (default: 1e10)

LOG_DIR="logs_no_short"
mkdir -p "$LOG_DIR"

# Create cgroup if needed
CGROUP_NAME="r_jobs_$$"
if command -v cgcreate &> /dev/null; then
  cgcreate -g memory:${CGROUP_NAME}
  cgset -r memory.limit_in_bytes=${MEM_LIMIT} ${CGROUP_NAME}
  use_cgroup=true
else
  echo "Warning: cgroups tools not found, falling back to ulimit"
  use_cgroup=false
fi

for G in "${GAMMA_LIST[@]}"; do
  LOG="${LOG_DIR}/no_short_${MODE}_gamma${G}_wealth${WEALTH}_$(date +%Y%m%d-%H%M%S).log"
  echo ">>>  gamma=${G}   mode=${MODE}   mem=${MEM_LIMIT}   wealth=${WEALTH}   → log ${LOG}"
  
  if $use_cgroup; then
    env GAMMA="${G}" IEF_MODE="${MODE}" WEALTH="${WEALTH}" \
      cgexec -g memory:${CGROUP_NAME} taskset -c "${CPU_LIST}" \
      Rscript 8_no_short.R >"${LOG}" 2>&1
  else
    ulimit -v $(numfmt --from=iec ${MEM_LIMIT})
    env GAMMA="${G}" IEF_MODE="${MODE}" WEALTH="${WEALTH}" \
      taskset -c "${CPU_LIST}" Rscript 8_no_short.R >"${LOG}" 2>&1
  fi
done

# Clean up cgroup if we created one
if $use_cgroup; then
  cgdelete memory:${CGROUP_NAME}
fi

echo "### all gamma runs completed ###"

# #!/usr/bin/env bash
# ###############################################################################
# #  Run Portfolio-ML jobs  — one per gamma, choose smoke | mini | prod
# ###############################################################################
# set -euo pipefail

# CPU_LIST="6-31"                     # pin to your free logical cores
# GAMMA_LIST=(1 5 10 20 100)          # frontier grid
# # GAMMA_LIST=(10 20 100)          # frontier grid
# MODE=${1:-mini}                     # default = mini
# MEM_LIMIT=${2:-8G}

# LOG_DIR="logs_no_short"
# mkdir -p "$LOG_DIR"

# for G in "${GAMMA_LIST[@]}"; do
#   LOG="${LOG_DIR}/no_short_${MODE}_gamma${G}_$(date +%Y%m%d-%H%M%S).log"
#   echo ">>>  gamma=${G}   mode=${MODE}   → log ${LOG}"
#   # env GAMMA="${G}" IEF_MODE="${MODE}" \
#   #     taskset -c "${CPU_LIST}" Rscript 8_no_short.R >"${LOG}" 2>&1
#   systemd-run --scope --user -p MemoryLimit=${MEM_LIMIT} \
#     env GAMMA="${G}" IEF_MODE="${MODE}" \
#     taskset -c "${CPU_LIST}" Rscript 8_no_short.R >"${LOG}" 2>&1
# done

# echo "### all gamma runs completed ###"
