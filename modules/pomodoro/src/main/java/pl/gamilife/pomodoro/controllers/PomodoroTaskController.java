package pl.gamilife.pomodoro.controllers;

import pl.gamilife.pomodoro.shared.ApiResponse;
import pl.gamilife.pomodoro.usecase.createpomodorotask.CreatePomodoroTaskRequest;
import pl.gamilife.pomodoro.usecase.createpomodorotask.CreatePomodoroTaskResponse;
import pl.gamilife.pomodoro.usecase.createpomodorotask.CreatePomodoroUseCase;
import pl.gamilife.pomodoro.usecase.deletepomodorotask.DeletePomodoroTaskUseCase;
import pl.gamilife.pomodoro.usecase.editpomodorotask.EditPomodoroTaskRequest;
import pl.gamilife.pomodoro.usecase.editpomodorotask.EditPomodoroTaskResponse;
import pl.gamilife.pomodoro.usecase.editpomodorotask.EditPomodoroTaskUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks")
public class PomodoroTaskController {

    private final CreatePomodoroUseCase createPomodoroUseCase;
    private final EditPomodoroTaskUseCase editPomodoroTaskUseCase;
    private final DeletePomodoroTaskUseCase deletePomodoroTaskUseCase;

    public PomodoroTaskController(CreatePomodoroUseCase createPomodoroUseCase,
                                  EditPomodoroTaskUseCase editPomodoroTaskUseCase,
                                  DeletePomodoroTaskUseCase deletePomodoroTaskUseCase) {
        this.createPomodoroUseCase = createPomodoroUseCase;
        this.editPomodoroTaskUseCase = editPomodoroTaskUseCase;
        this.deletePomodoroTaskUseCase = deletePomodoroTaskUseCase;
    }

    @PostMapping("/{taskId}/pomodoro-tasks")
    public ResponseEntity<CreatePomodoroTaskResponse> createPomodoroTask(
            @PathVariable("taskId") UUID taskId,
            @RequestBody @Valid CreatePomodoroTaskRequest request) {

        CreatePomodoroTaskResponse response = createPomodoroUseCase.execute(taskId, request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }


    @PutMapping("/pomodoro-tasks/{pomodoroId}")
    public ResponseEntity<EditPomodoroTaskResponse> editPomodoroTask(
            @PathVariable("pomodoroId") UUID pomodoroId,
            @RequestBody @Valid EditPomodoroTaskRequest request) {

        EditPomodoroTaskResponse response = editPomodoroTaskUseCase.execute(pomodoroId, request);
        return ResponseEntity.ok(response);
    }


    @DeleteMapping("/pomodoro-tasks/{pomodoroTaskId}")
    public ResponseEntity<ApiResponse> deletePomodoroTask(
            @PathVariable("pomodoroTaskId") UUID pomodoroTaskId) {

        deletePomodoroTaskUseCase.execute(pomodoroTaskId);
        return ResponseEntity.ok(
                new ApiResponse("Pomodoro Task with id: " + pomodoroTaskId + " deleted successfully"));
    }
}
