-- Run from this plugin's root: nvim --headless -u NONE -l tests/launch.lua
-- Needs replconnect.nvim: set REPLCONNECT_CORE to its checkout (default: a sibling REPLConnect.nvim).
local core = vim.env.REPLCONNECT_CORE or vim.fs.joinpath(vim.fs.dirname(vim.fn.getcwd()), "REPLConnect.nvim")
assert(vim.uv.fs_stat(core), "replconnect.nvim not found at " .. core .. "; set REPLCONNECT_CORE")
vim.opt.runtimepath:prepend(core)
vim.opt.runtimepath:prepend(vim.fn.getcwd())
-- Load this plugin as Neovim would.
vim.cmd.runtime({ args = { "plugin/*.lua" }, bang = true })
local config = require("replconnect.config")
local repl = require("replconnect.repl")
local languages = require("replconnect.languages")

local function eq(a, b)
	assert(vim.deep_equal(a, b), vim.inspect(a) .. " ~= " .. vim.inspect(b))
end

local r = require("replconnect.r")

-- the plugin registers itself with the core, and is found by filetype
assert(languages.get("r") == r)
assert(languages.for_filetype("r") == r and languages.for_filetype("rmd") == r)
eq(r.command, { "arf" })
config.set()
eq(config.repl_options(r).command, { "arf" })

-- the plugin root contains the R package
assert(vim.uv.fs_stat(vim.fs.joinpath(r.root(), "replconnect", "DESCRIPTION")), r.root())

-- R in arf: a generated profile chains the user's profile, then starts the service
eq(r.r_string('a"b\\c'), '"a\\"b\\\\c"')
local profile = r.profile_code("/x/y")
assert(profile:find('Sys.getenv("REPLCONNECT_R_PROFILE_USER")', 1, true))
assert(profile:find("source(profile, local = globalenv())", 1, true))
assert(profile:find("if (interactive()) local({", 1, true))
assert(profile:find('requireNamespace("replconnect", quietly = TRUE)', 1, true))
assert(profile:find("    replconnect::start()\n", 1, true))
assert(profile:find('\\"/x/y/replconnect\\"', 1, true), profile)
assert(not profile:find("capture_plots", 1, true))
assert(
	r.profile_code("/x/y", { capture_plots = true, plot_limit = 7 })
		:find("replconnect::capture_plots(TRUE, limit = 7)", 1, true)
)
assert(not pcall(r.profile_code, "/x/y", { capture_plots = true, plot_limit = 0 }))
local profile_path = vim.fn.tempname() .. ".R"
local r_cmd, r_env = r.build_cmd({ command = { "arf" } }, { "--with-r-version", "4.4" }, "/x/y", profile_path)
eq(r_cmd, { "arf", "--with-r-version", "4.4" })
eq(r_env.R_PROFILE_USER, profile_path)
eq(table.concat(vim.fn.readfile(profile_path), "\n"), profile)
eq(r_env.REPLCONNECT_R_PROFILE_USER, vim.env.R_PROFILE_USER or "")
local generated, generated_env = repl.build_cmd(r, { command = { "arf" } })
eq(generated, { "arf" })
assert(vim.uv.fs_stat(generated_env.R_PROFILE_USER), "the profile is written to a temporary file")
vim.fn.delete(profile_path)
vim.fn.delete(generated_env.R_PROFILE_USER)

-- Plot capture is forwarded into the generated profile.
config.set({ repl = { capture_plots = true, size = 9 } })
eq(r.plot_capture, true)
eq(repl.launch_options(r).capture_plots, true)
r.build_cmd(repl.launch_options(r), nil, "/x/y", profile_path)
assert(table.concat(vim.fn.readfile(profile_path), "\n"):find("capture_plots", 1, true))
vim.fn.delete(profile_path)
config.set()

-- a missing executable is reported against repl.r.command
local notified
local original = vim.notify
vim.notify = function(msg, level)
	notified = { msg = msg, level = level }
end
config.set({ repl = { r = { command = { "definitely-not-arf" } } } })
assert(repl.start("r") == nil)
assert(notified.msg:find("repl.r.command", 1, true), notified.msg)
vim.notify = original
config.set()

-- setup() (lazy.nvim `opts`) sets this plugin's launch options and keeps it registered
r.setup({ command = { "arf", "--vanilla" } })
assert(languages.get("r") == r)
eq(config.repl_options(r).command, { "arf", "--vanilla" })
r.setup()
eq(config.repl_options(r).command, { "arf" })

print("replconnect-r launch tests passed")
