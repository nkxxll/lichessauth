// The new component to be used when no game is selected.
// It receives a 'message' prop to display to the user.
export default function GameSelectionMessage({ message }: { message: string }) {
  return (
    <div className="flex items-center justify-center min-h-full p-8">
      <div className="bg-gradient-to-br from-blue-950 to-blue-900 rounded-2xl shadow-2xl p-8 sm:p-12 w-full max-w-2xl text-center border border-slate-700">
        <h2 className="text-xl sm:text-2xl font-semibold text-slate-200 tracking-tight">
          {message}
        </h2>
      </div>
    </div>
  );
}
