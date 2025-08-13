import type { Arrow } from "react-chessboard";
import type {
  OcamlArrow,
  OcamlChessboardOptions,
} from "../interfaces/chess.ts";
import type { OpeningProps } from "../components/Opening.tsx";

export function OArrowToArrow(arrow: OcamlArrow): Arrow {
  return {
    startSquare: arrow.start_square,
    endSquare: arrow.end_square,
    color: arrow.color[0],
  };
}

export function OChessBoradOptionToChessBoardOptions(
  opt: OcamlChessboardOptions,
): OpeningProps {
  const [upperCaseOrientation] = opt.orientation;
  const orientation: "white" | "black" = upperCaseOrientation.toLowerCase() as
    | "white"
    | "black";
  return {
    fen: opt.fen,
    orientation,
    badMove: OArrowToArrow(opt.bad_move),
    goodMove: OArrowToArrow(opt.good_move),
  };
}
