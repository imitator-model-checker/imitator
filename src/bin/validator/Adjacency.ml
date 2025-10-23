  let generate ~sampler ~nodes ~density  =
    let matrix : bool array array = Array.make_matrix nodes nodes false in 

    (* Spanning tree *)
    let parents = ref [0] in
    for i = 1 to nodes - 1 do 
      let parent = Sampler.sample_uniform sampler ~from:!parents in
      matrix.(parent).(i) <- true;
      parents := i::!parents
    done;
    
    let transform = fun b ->  b || Sampler.next_bool sampler ~prob:density in 
    Array.map (Array.map transform) matrix