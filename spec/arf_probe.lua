-- End-to-end busy-status test against a fresh, real arf console.
-- Run after `just r_tests`: just arf_probe
-- No user's R profile, pane, focus event, polling config or nc involved.
-- Needs replconnect.nvim: set REPLCONNECT_CORE to its checkout (default: a sibling REPLConnect.nvim).
local core = vim.env.REPLCONNECT_CORE or vim.fs.joinpath(vim.fs.dirname(vim.fn.getcwd()), "REPLConnect.nvim")
assert(vim.uv.fs_stat(core), "replconnect.nvim not found at " .. core .. "; set REPLCONNECT_CORE")
vim.opt.runtimepath:prepend(core)
vim.opt.runtimepath:prepend(vim.fn.getcwd())
local uv = vim.uv
assert(vim.fn.executable("arf") == 1, "arf is not on PATH")
local dir = "/tmp/ri-arf-probe-" .. uv.os_getpid()
assert(vim.fn.mkdir(dir, "p", 448) == 1, "could not create private probe directory " .. dir)
local profile = dir .. "/profile.R"
vim.fn.writefile({ "if (interactive()) replconnect::start()" }, profile)
local terminal = vim.api.nvim_create_buf(false, true)
local job, client
local function output()
	return vim.api.nvim_buf_is_valid(terminal) and vim.api.nvim_buf_get_lines(terminal, 0, -1, false) or {}
end
local function saw(marker)
	for _, line in ipairs(output()) do
		if vim.trim(line) == marker then return true end
