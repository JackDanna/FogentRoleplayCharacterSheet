/** @type {import('tailwindcss').Config} */
module.exports = {
    mode: "jit",
    content: [
        "./index.html",
        "./**/*.{fs,js,ts,jsx,tsx}",
    ],
    theme: {
        extend: {},
    },
    plugins: []
    //plugins: [require('daisyui')]
}
