package pl.gamilife.pomodoro.infrastructure.web;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.pomodoro.application.createpomodorotask.CreatePomodoroTaskRequest;
import pl.gamilife.pomodoro.application.createpomodorotask.CreatePomodoroTaskResponse;
import pl.gamilife.pomodoro.application.createpomodorotask.CreatePomodoroUseCase;
import pl.gamilife.pomodoro.application.deletepomodorotask.DeletePomodoroTaskUseCase;
import pl.gamilife.pomodoro.application.editpomodorotask.EditPomodoroTaskRequest;
import pl.gamilife.pomodoro.application.editpomodorotask.EditPomodoroTaskResponse;
import pl.gamilife.pomodoro.application.editpomodorotask.EditPomodoroTaskUseCase;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/pomodoro-items") // TODO: address changed
public class PomodoroItemController {

    private final CreatePomodoroUseCase createPomodoroUseCase;
    private final EditPomodoroTaskUseCase editPomodoroTaskUseCase;
    private final DeletePomodoroTaskUseCase deletePomodoroTaskUseCase;

    @PostMapping
    public ResponseEntity<CreatePomodoroTaskResponse> createPomodoroTask(
            @RequestBody @Valid CreatePomodoroTaskRequest request
    ) {
        CreatePomodoroTaskResponse response = createPomodoroUseCase.execute(null, request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{pomodoroId}")
    public ResponseEntity<EditPomodoroTaskResponse> editPomodoroTask(
            @PathVariable("pomodoroId") UUID pomodoroId,
            @RequestBody @Valid EditPomodoroTaskRequest request) {

        EditPomodoroTaskResponse response = editPomodoroTaskUseCase.execute(pomodoroId, request);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{pomodoroTaskId}")
    public ResponseEntity<ApiResponse> deletePomodoroTask(@PathVariable("pomodoroTaskId") UUID pomodoroTaskId) {

        deletePomodoroTaskUseCase.execute(pomodoroTaskId);
        return ResponseEntity.ok(
                new ApiResponse("Pomodoro Task with id: " + pomodoroTaskId + " deleted successfully"));
    }
}
