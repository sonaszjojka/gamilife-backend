package edu.pjwstk.pomodoro.editpomodorotask;


import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks/pomodoro-tasks")
public class EditPomodoroTaskController {
    private final EditPomodoroTaskUseCase editPomodoroTaskUseCase;

    public EditPomodoroTaskController(EditPomodoroTaskUseCase editPomodoroTaskUseCase)
    {
        this.editPomodoroTaskUseCase = editPomodoroTaskUseCase;
    }

    @PutMapping("/{pomodoroId}")
    public ResponseEntity <EditPomodoroTaskResponse> editPomodoroTask(@RequestBody @Valid EditPomodoroTaskRequest request, @PathVariable("pomodoroId") UUID pomodoroId)
    {
        EditPomodoroTaskResponse response = editPomodoroTaskUseCase.execute(pomodoroId, request);
        return ResponseEntity.ok(response);
    }
}
