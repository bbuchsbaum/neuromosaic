const { defineConfig } = require("@playwright/test");

module.exports = defineConfig({
  testDir: "./e2e",
  globalSetup: require.resolve("./e2e/global-setup.cjs"),
  outputDir: "test-results",
  fullyParallel: false,
  workers: 1,
  forbidOnly: Boolean(process.env.CI),
  retries: process.env.CI ? 1 : 0,
  reporter: [
    ["line"],
    ["html", { outputFolder: "playwright-report", open: "never" }]
  ],
  webServer: {
    command: "node e2e/serve-artifacts.cjs",
    url: "http://127.0.0.1:4173/health",
    reuseExistingServer: false,
    timeout: 20_000
  },
  use: {
    browserName: "chromium",
    viewport: { width: 1440, height: 900 },
    colorScheme: "light",
    locale: "en-CA",
    screenshot: "only-on-failure",
    trace: "retain-on-failure"
  }
});
