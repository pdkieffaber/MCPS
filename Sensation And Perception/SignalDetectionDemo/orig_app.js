const canvas = document.getElementById("scene");
const ctx = canvas.getContext("2d");

const startBtn = document.getElementById("startBtn");
const downloadBtn = document.getElementById("downloadBtn");
const statusEl = document.getElementById("status");
const summaryEl = document.getElementById("summary");

const W = canvas.width;
const H = canvas.height;
const CX = W / 2;
const CY = H / 2;
const FIELD_RADIUS = 260;

const N_PLANES = 8;
const TRIAL_DURATION_MS = 1200;
const ITI_MS = 700;
const FIXATION_MS = 500;
const RESPONSE_WINDOW_MS = 2500;

const TRIALS_PER_BLOCK = 40;
const BLOCKS = [1, 2, 3];

// Signal prevalence by block
const SIGNAL_PROBABILITY_BY_BLOCK = {
  1: 0.5,
  2: 0.5,
  3: 0.2
};

let planes = [];
let animationId = null;
let running = false;
let awaitingResponse = false;

let experimentRunning = false;
let experimentTrials = [];
let currentTrialIndex = -1;
let currentTrial = null;
let stimOnsetTime = null;
let stimOffsetTime = null;
let responseTimeoutId = null;
let itiTimeoutId = null;
let fixationTimeoutId = null;

let waitingForBlockStart = false;
let currentBlockShown = null;

const results = [];

function rand(a, b) {
  return a + Math.random() * (b - a);
}

function randomPointInCircle(radius) {
  while (true) {
    const x = rand(-radius, radius);
    const y = rand(-radius, radius);
    if (x * x + y * y <= radius * radius) return { x, y };
  }
}

function respawnInCircle(obj, radius) {
  const p = randomPointInCircle(radius);
  obj.x = p.x;
  obj.y = p.y;
}

function buildExperimentTrials() {
  const trials = [];

  for (const block of BLOCKS) {
    for (let i = 0; i < TRIALS_PER_BLOCK; i++) {
      const signalProbability = SIGNAL_PROBABILITY_BY_BLOCK[block] ?? 0.5;
      const signalPresent = Math.random() < signalProbability;

      let baseSpeed = 70;
      let targetSpeed = 95;
      let instruction = "Respond as accurately as possible.";

      if (block === 2) {
        targetSpeed = 145;
        instruction = "The task is the same. One airplane may be moving faster. Respond as accurately as possible.";
      }

      if (block === 3) {
        targetSpeed = 95;
        instruction = "Missing a faster airplane is costly. If unsure, lean toward saying signal present.";
      }

      trials.push({
        block,
        trialNumberWithinBlock: i + 1,
        signalPresent,
        signalProbability,
        baseSpeed,
        targetSpeed,
        instruction
      });
    }
  }

  return trials;
}

function createPlanes(trial) {
  const arr = [];
  const targetIndex = trial.signalPresent
    ? Math.floor(Math.random() * N_PLANES)
    : -1;

  for (let i = 0; i < N_PLANES; i++) {
    const p = randomPointInCircle(FIELD_RADIUS);
    const angle = rand(0, Math.PI * 2);
    const speed = i === targetIndex ? trial.targetSpeed : trial.baseSpeed;

    arr.push({
      x: p.x,
      y: p.y,
      angle,
      speed,
      vx: Math.cos(angle) * speed,
      vy: Math.sin(angle) * speed,
      isTarget: i === targetIndex
    });
  }

  return arr;
}

function clearScene() {
  ctx.fillStyle = "black";
  ctx.fillRect(0, 0, W, H);
}

function drawFieldBoundary() {
  ctx.save();
  ctx.strokeStyle = "#333";
  ctx.lineWidth = 1.5;
  ctx.beginPath();
  ctx.arc(CX, CY, FIELD_RADIUS, 0, Math.PI * 2);
  ctx.stroke();
  ctx.restore();
}

function drawFixation() {
  clearScene();
  drawFieldBoundary();

  ctx.save();
  ctx.translate(CX, CY);
  ctx.strokeStyle = "white";
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.moveTo(-12, 0);
  ctx.lineTo(12, 0);
  ctx.moveTo(0, -12);
  ctx.lineTo(0, 12);
  ctx.stroke();
  ctx.restore();
}

function drawCenteredText(lines) {
  clearScene();
  drawFieldBoundary();

  ctx.save();
  ctx.fillStyle = "white";
  ctx.textAlign = "center";
  ctx.textBaseline = "middle";

  const fontSize = 28;
  const lineHeight = 40;
  ctx.font = `${fontSize}px Arial`;

  const startY = CY - ((lines.length - 1) * lineHeight) / 2;

  lines.forEach((line, i) => {
    ctx.fillText(line, CX, startY + i * lineHeight);
  });

  ctx.restore();
}

function drawAirplane(x, y, angle, scale = 1.15) {
  ctx.save();
  ctx.translate(CX + x, CY + y);
  ctx.rotate(angle);
  ctx.scale(scale, scale);
  ctx.fillStyle = "white";

  ctx.beginPath();
  ctx.moveTo(14, 0);
  ctx.lineTo(0, -5);
  ctx.lineTo(-7, -12);
  ctx.lineTo(-4, -4);
  ctx.lineTo(-17, -4);
  ctx.lineTo(-17, 4);
  ctx.lineTo(-4, 4);
  ctx.lineTo(-7, 12);
  ctx.lineTo(0, 5);
  ctx.closePath();
  ctx.fill();

  ctx.restore();
}

function drawPlanes() {
  clearScene();
  drawFieldBoundary();

  for (const p of planes) {
    drawAirplane(p.x, p.y, p.angle);
  }
}

function updatePlanes(dt) {
  for (const p of planes) {
    p.x += p.vx * dt;
    p.y += p.vy * dt;

    if (p.x * p.x + p.y * p.y > FIELD_RADIUS * FIELD_RADIUS) {
      respawnInCircle(p, FIELD_RADIUS);
    }
  }
}

function animate(startTime, lastTime) {
  animationId = requestAnimationFrame((now) => {
    if (!running) return;

    const dt = (now - lastTime) / 1000;
    const elapsed = now - startTime;

    updatePlanes(dt);
    drawPlanes();

    if (elapsed >= TRIAL_DURATION_MS) {
      endStimulus();
      return;
    }

    animate(startTime, now);
  });
}

function startStimulus() {
  planes = createPlanes(currentTrial);
  running = true;
  awaitingResponse = false;
  stimOnsetTime = performance.now();
  stimOffsetTime = null;
  drawPlanes();

  const startTime = performance.now();
  animate(startTime, startTime);
}

function endStimulus() {
  running = false;
  awaitingResponse = true;
  stimOffsetTime = performance.now();

  if (animationId !== null) {
    cancelAnimationFrame(animationId);
    animationId = null;
  }

  clearScene();
  drawFieldBoundary();

  statusEl.textContent =
    `Block ${currentTrial.block}, Trial ${currentTrial.trialNumberWithinBlock}: Respond now. F = signal present, J = no signal`;

  responseTimeoutId = setTimeout(() => {
    if (awaitingResponse) {
      submitResponse(null);
    }
  }, RESPONSE_WINDOW_MS);
}

function showBlockInstructions(block) {
  waitingForBlockStart = true;
  currentBlockShown = block;

  let lines = [];

  if (block === 1) {
    lines = [
      "Block 1",
      "",
      "Press F if one airplane was faster.",
      "Press J if all airplanes were the same.",
      "",
      "Respond as accurately as possible.",
      "",
      "Press SPACE to begin."
    ];
  } else if (block === 2) {
    lines = [
      "Block 2",
      "",
      "One airplane may be moving faster.",
      "",
      "Press F = one airplane was faster",
      "Press J = all airplanes were the same",
      "",
      "Press SPACE to begin."
    ];
  } else if (block === 3) {
    lines = [
      "Block 3",
      "",
      "Missing a faster airplane is very costly.",
      "",
      "Press F = one airplane was faster",
      "Press J = all airplanes were the same",
      "",
      "Press SPACE to begin."
    ];
  }

  drawCenteredText(lines);
  statusEl.textContent = `Block ${block}. Press SPACE to continue.`;
}

function startNextTrial() {
  currentTrialIndex += 1;

  if (currentTrialIndex >= experimentTrials.length) {
    endExperiment();
    return;
  }

  currentTrial = experimentTrials[currentTrialIndex];

  if (currentTrial.block !== currentBlockShown) {
    showBlockInstructions(currentTrial.block);
    return;
  }

  drawFixation();
  fixationTimeoutId = setTimeout(() => {
    startStimulus();
  }, FIXATION_MS);
}

function submitResponse(key) {
  if ((!running && !awaitingResponse) || !currentTrial) return;

  const responseTime = performance.now();
  const respondedDuringStimulus = running;

  running = false;
  awaitingResponse = false;

  if (animationId !== null) {
    cancelAnimationFrame(animationId);
    animationId = null;
  }

  if (responseTimeoutId) {
    clearTimeout(responseTimeoutId);
    responseTimeoutId = null;
  }

  let rt = null;
  if (key !== null) {
    const rtStart = stimOnsetTime ?? stimOffsetTime;
    rt = rtStart != null ? responseTime - rtStart : null;
  }

  let responseLabel = null;
  if (key === "f") responseLabel = "signal";
  if (key === "j") responseLabel = "no_signal";

  const correctResponse = currentTrial.signalPresent ? "signal" : "no_signal";
  const accuracy = responseLabel === correctResponse ? 1 : 0;

  clearScene();
  drawFieldBoundary();

  results.push({
    block: currentTrial.block,
    trial: currentTrial.trialNumberWithinBlock,
    signalPresent: currentTrial.signalPresent ? 1 : 0,
    signalProbability: currentTrial.signalProbability,
    response: responseLabel,
    correctResponse,
    accuracy,
    rtMs: rt,
    respondedDuringStimulus: respondedDuringStimulus ? 1 : 0,
    baseSpeed: currentTrial.baseSpeed,
    targetSpeed: currentTrial.signalPresent ? currentTrial.targetSpeed : currentTrial.baseSpeed
  });

  itiTimeoutId = setTimeout(() => {
    startNextTrial();
  }, ITI_MS);
}
function makeCSV(rows) {
  const headers = [
    "block",
    "trial",
    "signalPresent",
    "signalProbability",
    "response",
    "correctResponse",
    "accuracy",
    "rtMs",
    "respondedDuringStimulus",
    "baseSpeed",
    "targetSpeed"
  ];

  const csvLines = [headers.join(",")];

  rows.forEach(row => {
    const vals = headers.map(h => {
      const v = row[h];
      if (v === null || v === undefined) return "";
      return String(v);
    });
    csvLines.push(vals.join(","));
  });

  return csvLines.join("\n");
}

function downloadCSV() {
  const csv = makeCSV(results);
  const blob = new Blob([csv], { type: "text/csv;charset=utf-8;" });
  const url = URL.createObjectURL(blob);

  const a = document.createElement("a");
  a.href = url;
  a.download = "airplane_signal_detection_raw_data.csv";
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);

  URL.revokeObjectURL(url);
}

function endExperiment() {
  experimentRunning = false;
  waitingForBlockStart = false;
  awaitingResponse = false;
  running = false;

  clearScene();
  drawFieldBoundary();
  drawCenteredText([
    "Experiment complete.",
    "",
    "Click the button below to download your raw data.",
    ""
  ]);

  statusEl.textContent = "Experiment complete.";
  summaryEl.textContent = "";
  downloadBtn.style.display = "inline-block";
  downloadBtn.disabled = false;

  // Optional convenience: start download automatically as well.
  downloadCSV();
}

function startExperiment() {
  if (experimentRunning) return;

  if (animationId !== null) {
    cancelAnimationFrame(animationId);
    animationId = null;
  }
  if (responseTimeoutId) clearTimeout(responseTimeoutId);
  if (itiTimeoutId) clearTimeout(itiTimeoutId);
  if (fixationTimeoutId) clearTimeout(fixationTimeoutId);

  results.length = 0;
  summaryEl.textContent = "";
  statusEl.textContent = "";
  downloadBtn.style.display = "none";
  downloadBtn.disabled = false;

  experimentTrials = buildExperimentTrials();
  currentTrialIndex = -1;
  currentTrial = null;
  currentBlockShown = null;
  experimentRunning = true;
  waitingForBlockStart = false;

  startNextTrial();
}

document.addEventListener("keydown", (e) => {
  if (!experimentRunning) return;

  if (waitingForBlockStart && e.code === "Space") {
    waitingForBlockStart = false;
    statusEl.textContent = "";
    drawFixation();
    fixationTimeoutId = setTimeout(() => {
      startStimulus();
    }, FIXATION_MS);
    return;
  }

  if (!waitingForBlockStart) {
    if (e.key === "f" || e.key === "F") submitResponse("f");
    if (e.key === "j" || e.key === "J") submitResponse("j");
  }
});

startBtn.addEventListener("click", startExperiment);
downloadBtn.addEventListener("click", downloadCSV);

clearScene();
drawFieldBoundary();
