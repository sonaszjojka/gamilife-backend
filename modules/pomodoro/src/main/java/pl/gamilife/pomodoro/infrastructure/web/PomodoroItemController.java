package pl.gamilife.pomodoro.infrastructure.web;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.pomodoro.application.createpomodoroitem.CreatePomodoroItemCommand;
import pl.gamilife.pomodoro.application.createpomodoroitem.CreatePomodoroItemResult;
import pl.gamilife.pomodoro.application.createpomodoroitem.CreatePomodoroItemUseCase;
import pl.gamilife.pomodoro.application.deletepomodorotask.DeletePomodoroTaskUseCase;
import pl.gamilife.pomodoro.application.editpomodorotask.EditPomodoroTaskRequest;
import pl.gamilife.pomodoro.application.editpomodorotask.EditPomodoroTaskResponse;
import pl.gamilife.pomodoro.application.editpomodorotask.EditPomodoroTaskUseCase;
import pl.gamilife.pomodoro.infrastructure.web.request.CreatePomodoroItemRequest;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/pomodoro-items") // TODO: address changed
public class PomodoroItemController {

    private final CreatePomodoroItemUseCase createPomodoroItemUseCase;
    private final EditPomodoroTaskUseCase editPomodoroTaskUseCase;
    private final DeletePomodoroTaskUseCase deletePomodoroTaskUseCase;

    @PostMapping
    public ResponseEntity<CreatePomodoroItemResult> createPomodoroTask(
            @CurrentUserId UUID userId,
            @RequestBody @Valid CreatePomodoroItemRequest request
    ) {
        CreatePomodoroItemResult response = createPomodoroItemUseCase.execute(new CreatePomodoroItemCommand(
                userId,
                request.taskId(),
                request.habitId(),
                request.cyclesRequired()
        ));
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{pomodoroId}")
    public ResponseEntity<EditPomodoroTaskResponse> editPomodoroTask(
            @CurrentUserId UUID userId,
            @PathVariable("pomodoroId") UUID pomodoroId,
            @RequestBody @Valid EditPomodoroTaskRequest request) {

        EditPomodoroTaskResponse response = editPomodoroTaskUseCase.execute(pomodoroId, request);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{pomodoroTaskId}")
    public ResponseEntity<ApiResponse> deletePomodoroTask(
            @CurrentUserId UUID userId,
            @PathVariable("pomodoroTaskId") UUID pomodoroTaskId
    ) {

        deletePomodoroTaskUseCase.execute(pomodoroTaskId);
        return ResponseEntity.ok(
                new ApiResponse("Pomodoro Task with id: " + pomodoroTaskId + " deleted successfully"));
    }
}
