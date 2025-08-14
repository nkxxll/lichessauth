import { Chess } from "chess.js";
import { type ChessboardOptions, Chessboard } from "react-chessboard";
import toast from "react-hot-toast";

export type OpeningProps = {
  baseMoves: string[];
  badMove: string;
  goodMove: string;
  orientation: "white" | "black";
};

function pgnFromBase(baseMoves: Array<string>) {
  let count = 1;
  let res = [];
  let i = 1;
  for (i; i < baseMoves.length; i += 2) {
    const m1 = baseMoves[i - 1];
    const m2 = baseMoves[i];
    res.push(`${count}. ${m1} ${m2}`);
    count++;
  }
  if (i + 1 != baseMoves.length) {
    res.push(`${count}. ${baseMoves[baseMoves.length - 1]}`);
  }
  return res.join(" ");
}

// New component for displaying and copying chess moves
const MoveAnalysisButtons = ({
  baseMoves,
  badMove,
  goodMove,
}: {
  baseMoves: string[];
  badMove: string;
  goodMove: string;
}) => {
  // Function to copy text to clipboard with a fallback
  const copyToClipboard = (text: string) => {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).then(() => {
        toast("Copied Moves!");
      });
    } else {
      // Fallback for environments where clipboard API is not available (like the Canvas iframe)
      const el = document.createElement("textarea");
      el.value = text;
      document.body.appendChild(el);
      el.select();
      document.execCommand("copy");
      document.body.removeChild(el);
      toast("Copied Moves!");
    }
  };

  return (
    <div className="flex flex-col items-center justify-center p-4 text-center space-y-4 h-full">
      <div className="bg-gradient-to-br from-blue-950 to-blue-900 rounded-2xl shadow-2xl p-4 sm:p-8 w-full max-w-9/10 border border-slate-700">
        <h3 className="text-xl font-semibold text-slate-200 mb-3">
          Move Analysis
        </h3>

        {/* The two buttons are now side-by-side in a flex container */}
        <div className="flex flex-row mb-3 gap-4">
          {/* Actual Moves Button */}
          <button
            onClick={() =>
              copyToClipboard(pgnFromBase([...baseMoves, badMove]))
            }
            className="sm:w-auto px-6 py-3 rounded-full bg-slate-700 text-slate-200 font-medium hover:bg-slate-600 transition-colors duration-200 shadow-md flex-grow"
          >
            Actual Moves:{" "}
            <span className="font-mono">
              {pgnFromBase([...baseMoves, badMove])}
            </span>
          </button>

          {/* Correct Moves Button */}
          <button
            onClick={() =>
              copyToClipboard(pgnFromBase([...baseMoves, goodMove]))
            }
            className="sm:w-auto px-6 py-3 rounded-full bg-blue-600 text-white font-medium hover:bg-blue-700 transition-colors duration-200 shadow-md flex-grow"
          >
            Correct Moves:{" "}
            <span className="font-mono">
              {pgnFromBase([...baseMoves, goodMove])}
            </span>
          </button>
        </div>

        {/* Analysis Link */}
        <a
          href="https://lichess.org/analysis"
          target="_blank"
          rel="noopener noreferrer"
          className="text-blue-400 hover:text-blue-300 underline font-medium transition-colors duration-200"
        >
          Add to your PGN file... (click for analysis)
        </a>
      </div>
    </div>
  );
};

export function Opening({
  baseMoves,
  goodMove,
  badMove,
  orientation,
}: OpeningProps) {
  const chess_good = new Chess();
  const chess_bad = new Chess();
  for (let i = 0; i < baseMoves.length; i++) {
    const moveNotation = baseMoves[i];
    chess_bad.move(moveNotation);
  }
  for (let i = 0; i < baseMoves.length; i++) {
    const moveNotation = baseMoves[i];
    chess_good.move(moveNotation);
  }
  chess_bad.move(badMove);
  chess_good.move(goodMove);
  const config_bad: ChessboardOptions = {
    allowDragging: false,
    allowDragOffBoard: false,
    animationDurationInMs: 500,
    boardOrientation: orientation,
    clearArrowsOnClick: false,
    position: chess_bad.fen(),
    lightSquareStyle: {
      backgroundColor: "#3b82f6", // A lighter shade of blue
    },
    darkSquareStyle: {
      backgroundColor: "#1e40af", // A slightly darker, but still vibrant blue
    },
    boardStyle: {
      borderStyle: "rounded",
      borderRadius: "0.4rem",
    }
  };
  const config_good: ChessboardOptions = {
    allowDragging: false,
    allowDragOffBoard: false,
    animationDurationInMs: 500,
    boardOrientation: orientation,
    clearArrowsOnClick: false,
    position: chess_good.fen(),
    lightSquareStyle: {
      backgroundColor: "#3b82f6", // A lighter shade of blue
    },
    darkSquareStyle: {
      backgroundColor: "#1e40af", // A slightly darker, but still vibrant blue
    },
    boardStyle: {
      borderStyle: "rounded",
      borderRadius: "0.4rem",
    }
  };
  return (
    <div className="">
      <div className="pt-2 pr-2 flex gap-2">
        <div className="rounded-lg border-2 border-red-700 w-1/2">
          <Chessboard options={config_bad} />
        </div>
        <div className="rounded-lg border-2 border-green-700 w-1/2">
          <Chessboard options={config_good} />
        </div>
      </div>
      <MoveAnalysisButtons
        baseMoves={baseMoves}
        goodMove={goodMove}
        badMove={badMove}
      />
    </div>
  );
}
