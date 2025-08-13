import { useMutation } from "@tanstack/react-query";
import type { OpeningProps } from "../components/Opening";
import type { Game, OcamlChessboardOptions } from "../interfaces/chess";
import { OChessBoradOptionToChessBoardOptions } from "../utils/utils";

export async function fetchGameList(): Promise<Game[]> {
  const res = await fetch("/api/gamelist");

  if (!res.ok) {
    throw new Error(`Failed to fetch games: ${res.status}`);
  }

  return res.json();
}

export async function fetchOpeningError(
  color: "white" | "black",
  moves: string,
): Promise<OpeningProps> {
  const res = await fetch(
    `/api/opening?color=${color}&moves=${encodeURIComponent(moves)}`,
    {
      method: "GET",
      headers: {
        Accept: "application/json",
      },
    },
  );

  if (!res.ok) {
    throw new Error(
      `Failed to fetch opening error: ${res.status} ${res.statusText}`,
    );
  }

  const data: OcamlChessboardOptions = await res.json();

  return OChessBoradOptionToChessBoardOptions(data);
}

export function useOpeningErrorMutation() {
  return useMutation({
    mutationFn: ({
      color,
      moves,
    }: {
      color: "white" | "black";
      moves: string;
    }) => fetchOpeningError(color, moves),
  });
}
