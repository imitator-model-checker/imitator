(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Handling dynamic abstract model
 *
 * File contributors : Ta Quang Minh
 * Created           : 2026/08/22
 *
 ************************************************************)
type update_event =
  | Updated
  | Finished


class model_provider (model : AbstractModel.abstract_model) =
  object  (self)

    (* val mutex = Mutex.create ()
    val condition = Condition.create ()

    val mutable updated = false
    val mutable finished = false

    method get_model =
      model

    method update =
      Mutex.lock mutex;
      updated <- true;
      Condition.signal condition;
      Mutex.unlock mutex

    method finish =
      Mutex.lock mutex;
      finished <- true;
      Condition.signal condition;
      Mutex.unlock mutex

    method wait_for_update =
      Mutex.lock mutex;

      while not updated && not finished do
        Condition.wait condition mutex
      done;

      let event =
        if finished then
          Finished
        else
          Updated
      in

      updated <- false;

      Mutex.unlock mutex;

      event *)



    val mutable event = None


    method private read_key () =
      let old_settings = Unix.tcgetattr Unix.stdin in

      let new_settings = {
        old_settings with
        Unix.c_icanon = false;
        Unix.c_echo = false;
        Unix.c_vmin = 1;
        Unix.c_vtime = 0;
      } in

      Unix.tcsetattr Unix.stdin Unix.TCSANOW new_settings;

      let c = input_char stdin in

      Unix.tcsetattr Unix.stdin Unix.TCSANOW old_settings;

      c

    method wait_for_update =
      let c = self#read_key () in

      match c with
      | 'u' ->
          Updated

      | 'q' ->
          Finished

      | _ ->
          self#wait_for_update
  end
;;