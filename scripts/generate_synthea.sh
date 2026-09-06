#!/usr/bin/env bash
# ============================================================================
# generate_synthea.sh
# Regenerate the raw synthetic dataset used by this project.
#
# Requirements: Java 17+ (Synthea master builds are compiled for class 61).
# Output:       data/raw/{patients,encounters,conditions,organizations,payers,
#               providers}.csv
#
# The repo already ships a generated data/raw/ so this step is OPTIONAL --
# run it only if you want a fresh population or a different size / state.
# ============================================================================
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORK="${HERE}/synthea"
POP="${1:-2000}"
STATE="${2:-Massachusetts}"
SEED="${3:-20240101}"

mkdir -p "${WORK}"
cd "${WORK}"

if [ ! -f synthea-with-dependencies.jar ]; then
  echo "Downloading Synthea..."
  curl -L -o synthea-with-dependencies.jar \
    https://github.com/synthetichealth/synthea/releases/download/master-branch-latest/synthea-with-dependencies.jar
fi

echo "Generating ~${POP} living patients (${STATE}, seed ${SEED})..."
java -Xmx3500m -jar synthea-with-dependencies.jar \
  -s "${SEED}" -p "${POP}" \
  --exporter.csv.export true \
  --exporter.fhir.export false \
  "${STATE}"

echo "Copying the tables the ETL needs into data/raw/ ..."
mkdir -p "${HERE}/data/raw"
for t in patients encounters conditions organizations payers providers; do
  cp "output/csv/${t}.csv" "${HERE}/data/raw/${t}.csv"
done

echo "Done. Next:  Rscript data_prep.R"
