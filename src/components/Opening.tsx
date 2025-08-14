import { Chess } from "chess.js";
import { type ChessboardOptions, Chessboard } from "react-chessboard";

export type OpeningProps = {
  baseMoves: string[];
  badMove: string;
  goodMove: string;
  orientation: "white" | "black";
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
  };
  const config_good: ChessboardOptions = {
    allowDragging: false,
    allowDragOffBoard: false,
    animationDurationInMs: 500,
    boardOrientation: orientation,
    clearArrowsOnClick: false,
    position: chess_good.fen(),
  };
  return (
    <div className="fixed flex top-0 right-0 max-w-1/2">
      <div className="w-1/2">
        <Chessboard options={config_bad} />
      </div>
      <div className="w-1/2">
        <Chessboard options={config_good} />
      </div>
    </div>
  );
}
