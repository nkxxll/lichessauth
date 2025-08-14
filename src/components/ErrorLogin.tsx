export default function ErrorLogin() {
  return (
    <>
      <div className="dark:bg-slate-900 dark:text-slate-200 dark:from-purple-950 dark:to-blue-950 dark:bg-gradient-to-r rounded-2xl shadow-2xl p-8 sm:p-12 max-w-lg w-full text-center space-y-6 border border-slate-700">
        <svg
          className="mx-auto h-16 w-16 text-red-400 animate-pulse-slow"
          fill="none"
          stroke="currentColor"
          viewBox="0 0 24 24"
          xmlns="http://www.w3.org/2000/svg"
        >
          <path
            stroke-linecap="round"
            stroke-linejoin="round"
            stroke-width="2"
            d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
          ></path>
        </svg>

        <h1 className="text-3xl sm:text-4xl font-extrabold tracking-tight text-white">
          Oops! Something went wrong.
        </h1>

        <div id="error-message-container" className="space-y-4">
          <p id="error-message" className="text-slate-300 text-lg sm:text-xl">
            <span>
              An unexpected error occurred. Please try logging in again to
              resolve the issue.
            </span>
          </p>
          <a
            href="/api/login"
            className="inline-block px-8 py-3 mt-4 text-sm font-semibold text-white bg-blue-600 rounded-full shadow-lg hover:bg-blue-700 transition-all duration-300 ease-in-out transform hover:-translate-y-1"
          >
            Might have to log in
          </a>
        </div>
      </div>
    </>
  );
}
