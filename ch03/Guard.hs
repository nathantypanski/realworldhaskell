-- file: ch03/Guard.hs

fromMaybe defval wrapped =
  case wrapped of
    Nothing -> defval
    Just vl -> vl
