package pl.gamilife.pomodoro.infrastructure.web;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.pomodoro.application.createpomodoroitem.CreatePomodoroItemCommand;
import pl.gamilife.pomodoro.application.createpomodoroitem.CreatePomodoroItemResult;
import pl.gamilife.pomodoro.application.createpomodoroitem.CreatePomodoroItemUseCase;
import pl.gamilife.pomodoro.application.deletepomodoroitem.DeletePomodoroItemCommand;
import pl.gamilife.pomodoro.application.deletepomodoroitem.DeletePomodoroItemUseCase;
import pl.gamilife.pomodoro.application.editpomodoroitem.EditPomodoroItemCommand;
import pl.gamilife.pomodoro.application.editpomodoroitem.EditPomodoroItemResult;
import pl.gamilife.pomodoro.application.editpomodoroitem.EditPomodoroItemUseCase;
import pl.gamilife.pomodoro.infrastructure.web.request.CreatePomodoroItemRequest;
import pl.gamilife.pomodoro.infrastructure.web.request.EditPomodoroItemRequest;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;
import pl.gamilife.shared.web.util.annotation.CurrentUserTimezone;

import java.time.ZoneId;
import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/pomodoro-items") // TODO: address changed
public class PomodoroItemController {

    private final CreatePomodoroItemUseCase createPomodoroItemUseCase;
    private final EditPomodoroItemUseCase editPomodoroItemUseCase;
    private final DeletePomodoroItemUseCase deletePomodoroItemUseCase;

    @PostMapping
    public ResponseEntity<CreatePomodoroItemResult> createPomodoroTask(
            @CurrentUserId UUID userId,
            @CurrentUserTimezone ZoneId zoneId,
            @RequestBody @Valid CreatePomodoroItemRequest request
    ) {
        CreatePomodoroItemResult response = createPomodoroItemUseCase.execute(new CreatePomodoroItemCommand(
                userId,
                zoneId,
                request.taskId(),
                request.habitId(),
                request.cyclesRequired()
        ));
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{pomodoroId}")
    public ResponseEntity<EditPomodoroItemResult> editPomodoroTask(
            @CurrentUserId UUID userId,
            @CurrentUserTimezone ZoneId zoneId,
            @PathVariable("pomodoroId") UUID pomodoroId,
            @RequestBody @Valid EditPomodoroItemRequest request) {

        EditPomodoroItemResult response = editPomodoroItemUseCase.execute(new EditPomodoroItemCommand(
                userId,
                zoneId,
                pomodoroId,
                request.cyclesRequired(),
                request.completeCycles()
        ));
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{pomodoroTaskId}")
    public ResponseEntity<ApiResponse> deletePomodoroTask(
            @CurrentUserId UUID userId,
            @CurrentUserTimezone ZoneId zoneId,
            @PathVariable("pomodoroTaskId") UUID pomodoroTaskId
    ) {

        deletePomodoroItemUseCase.execute(new DeletePomodoroItemCommand(userId, zoneId, pomodoroTaskId));
        return ResponseEntity.ok(new ApiResponse("Pomodoro Task with id: " + pomodoroTaskId + " deleted successfully"));
    }
}
