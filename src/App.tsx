import toast, { Toaster } from "react-hot-toast";
import { useQuery, useQueryClient } from "@tanstack/react-query";
import { fetchGameList, useOpeningErrorMutation } from "./api/api";
import { Opening, type OpeningProps } from "./components/Opening";
import { useEffect, useState } from "react";
import ErrorLogin from "./components/ErrorLogin";
import GameSelectionMessage from "./components/GameSelectionMessage";
import type { Game } from "./interfaces/chess";

const PLAYER = "brandtheon";

function App() {
  // has to be at the top
  useEffect(() => {
    SearchQuery();
  }, []);

  const openingErrorMutation = useOpeningErrorMutation();

  function SearchQuery() {
    const params = new URLSearchParams(window.location.search);
    const logged = params.get("loggedin");
    if (logged === "true") {
      toast.success("Logged in");
    }
  }

  const [config, setConfig] = useState<OpeningProps | null>(null);

  const queryClient = useQueryClient();

  const { data, isLoading, error } = useQuery({
    queryKey: ["gameList"],
    queryFn: () => fetchGameList(100),
  });
  const [filteredGames, setFilteredGames] = useState<Game[] | undefined>([]);
  const [filterText, setFilterText] = useState("");

  useEffect(() => {
    const lowercasedFilter = filterText.toLowerCase();
    const filtered = data?.filter(
      (game) =>
        game.id.toLowerCase().includes(lowercasedFilter) ||
        game.opening.name.toLowerCase().includes(lowercasedFilter),
    );
    setFilteredGames(filtered);
  }, [filterText, data]);

  function refreshGameList() {
    queryClient.invalidateQueries({ queryKey: ["gameList"] });
    toast("Refreshed...");
  }

  if (isLoading) return <p>Loading games...</p>;
  if (error instanceof Error)
    return (
      <div className="flex items-center justify-center p-2 h-full bg-slate-950 text-slate-200 min-h-screen dark:bg-slate-900 dark:text-slate-200 dark:from-blue-950 dark:to-blue-900 dark:bg-gradient-to-r">
        <ErrorLogin />
      </div>
    );

  return (
    <div className="h-full w-full bg-slate-950 text-slate-200 min-h-screen dark:bg-slate-900 dark:text-slate-200 dark:from-blue-950 dark:to-blue-900 dark:bg-gradient-to-r">
      <Toaster />
      <div className="h-full w-full flex flex-row">
        <div className="max-h-[100vh] w-1/4 text-white p-2">
          <div className="max-h-full rounded-2xl p-2 shadow-2xl border border-slate-700 dark:bg-slate-900 dark:text-slate-200 dark:from-purple-950 dark:to-blue-950 dark:bg-gradient-to-r overflow-y-auto left-scrollbar scrollbar">
            <div className="mb-2 flex gap-2">
              <input
                type="text"
                id="filter-input"
                value={filterText}
                onChange={(e) => setFilterText(e.target.value)}
                placeholder="Filter Games by ID or Opening"
                className="w-full p-2 shadow-xl rounded-xl text-slate-200 border border-slate-700 focus:outline-none focus:ring-2 focus:ring-blue-500 transition-colors"
              />
            </div>
            <ul className="space-y-4">
              {filteredGames?.map((game) => {
                const color = game.players[0].id === PLAYER ? "white" : "black";
                const colorStyle =
                  color === "white"
                    ? "text-white bg-blue-500"
                    : "text-white bg-gray-800";
                return (
                  <li
                    key={game.id}
                    className="p-4 rounded-lg shadow-md hover:shadow-lg transition-shadow bg-gray-50 dark:bg-gray-900 flex flex-col md:flex-row md:items-center justify-between gap-4"
                  >
                    <div>
                      <p className="text-lg font-semibold">
                        {game.id} ({game.moves.length} moves)
                      </p>
                      <div className="flex items-center gap-2 mt-1">
                        <span
                          className={`px-2 py-1 rounded ${colorStyle} text-sm`}
                        >
                          {color}
                        </span>
                        <span className="font-bold text-sm">
                          {game.opening.name}
                        </span>
                      </div>
                    </div>
                    <button
                      className="mt-2 md:mt-0 p-2 border-2 rounded hover:bg-gray-200 dark:hover:bg-gray-700 transition-colors"
                      onClick={() => {
                        openingErrorMutation.mutate(
                          { color, moves: game.moves },
                          {
                            onSuccess: (data) => {
                              setConfig(data);
                            },
                          },
                        );
                      }}
                    >
                      Opening Error
                    </button>
                  </li>
                );
              })}
            </ul>
          </div>
        </div>
        <div className="w-3/4 text-black">
          {config ? (
            <Opening {...config} />
          ) : (
            <GameSelectionMessage message={"Choose a game to analyse"} />
          )}
        </div>
      </div>
    </div>
  );
}

export default App;
