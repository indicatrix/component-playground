import elmPlugin from 'vite-plugin-elm'
import tailwindcss from '@tailwindcss/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [
    // See https://www.npmjs.com/package/vite-plugin-elm
    elmPlugin(),

    // See https://tailwindcss.com/docs/installation/using-vite
    tailwindcss(),
  ],
})