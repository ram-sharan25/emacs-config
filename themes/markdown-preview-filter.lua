-- Remove raw HTML before Markdown is sent to a browser.  CSP is a second
-- layer of protection; this filter keeps untrusted markup out of the DOM.
function RawBlock(_)
  return {}
end

function RawInline(_)
  return {}
end

local function safe_link(target)
  local scheme = target:match("^([%a][%w+.-]*):")
  if not scheme then
    return not target:match("^//")
  end
  scheme = scheme:lower()
  return scheme == "http" or scheme == "https" or scheme == "mailto"
end

function Link(link)
  if safe_link(link.target) then
    return link
  end
  return link.content
end

function Image(image)
  if image.src:match("^data:image/") then
    return image
  end
  return image.caption
end
