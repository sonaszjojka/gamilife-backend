package edu.pjwstk.pomodoro.createpomodorotask;


import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks/{taskId}/pomodoro-tasks")
public class CreatePomodoroTaskController {

    private final CreatePomodoroUseCase createPomodoroUseCase;

    public CreatePomodoroTaskController(CreatePomodoroUseCase createPomodoroUseCase) {
        this.createPomodoroUseCase = createPomodoroUseCase;
    }

    @PostMapping
    public ResponseEntity<CreatePomodoroTaskResponse> save(@PathVariable("taskId") UUID taskId,
                                                           @RequestBody @Valid CreatePomodoroTaskRequest request){
        CreatePomodoroTaskResponse response = createPomodoroUseCase.execute(taskId, request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
}
