

module Std = struct
  include Bap_report_types
  module Docker = Bap_report_docker

  module Recipe = Bap_report_recipe
  type recipe = Recipe.t
  module Size = Bap_report_size
  module Read = Bap_report_read
  module View = Bap_report_view
  module Template = Bap_report_template

end
