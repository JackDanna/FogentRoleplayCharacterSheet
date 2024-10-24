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
    daisyui: {
        themes: ["cupcake","cyberpunk","fantasy"]
    },
    plugins: [require('daisyui')]
}
