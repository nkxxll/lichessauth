import {
  type Arrow,
  type ChessboardOptions,
  Chessboard,
} from "react-chessboard";

export type OpeningProps = {
  fen: string;
  badMove: Arrow;
  goodMove: Arrow;
  orientation: "white" | "black";
};

export function Opening({ fen, badMove, goodMove, orientation }: OpeningProps) {
  const config: ChessboardOptions = {
    allowDragging: false,
    allowDragOffBoard: false,
    animationDurationInMs: 500,
    arrows: [badMove, goodMove],
    boardOrientation: orientation,
    clearArrowsOnClick: false,
    position: fen,
  };
  return <div className="fixed top-0 right-0 max-w-1/2"><Chessboard options={config} /></div>
}
