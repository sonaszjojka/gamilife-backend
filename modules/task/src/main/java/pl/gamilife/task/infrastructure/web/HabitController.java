package pl.gamilife.task.infrastructure.web;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;
import pl.gamilife.shared.web.util.annotation.CurrentUserTimezone;
import pl.gamilife.task.application.createhabit.CreateHabitCommand;
import pl.gamilife.task.application.createhabit.CreateHabitResult;
import pl.gamilife.task.application.createhabit.CreateHabitUseCase;
import pl.gamilife.task.application.deletehabit.DeleteHabitCommand;
import pl.gamilife.task.application.deletehabit.DeleteHabitUseCase;
import pl.gamilife.task.application.edithabit.EditHabitCommand;
import pl.gamilife.task.application.edithabit.EditHabitUseCase;
import pl.gamilife.task.infrastructure.web.request.CreateHabitRequest;
import pl.gamilife.task.infrastructure.web.request.EditHabitRequest;
import pl.gamilife.task.infrastructure.web.response.ApiResponse;
import pl.gamilife.task.infrastructure.web.response.EditHabitResponse;

import java.time.ZoneId;
import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/habits") // TODO: Changed
public class HabitController {

    private final CreateHabitUseCase createHabitUseCase;
    private final EditHabitUseCase editHabitUseCase;
    private final DeleteHabitUseCase deleteHabitUseCase;

    @PostMapping
    public ResponseEntity<CreateHabitResult> create(
            @CurrentUserId UUID userId,
            @CurrentUserTimezone ZoneId zoneId,
            @RequestBody @Valid CreateHabitRequest request
    ) {
        CreateHabitResult response = createHabitUseCase.execute(new CreateHabitCommand(
                userId,
                zoneId,
                request.title(),
                request.description(),
                request.categoryId(),
                request.difficultyId(),
                request.cycleLength()
        ));
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PatchMapping("/{habitId}") // TODO: CHANGED TO PATCH AND REMOVED ID
    public ResponseEntity<EditHabitResponse> edit(
            @CurrentUserId UUID userId,
            @CurrentUserTimezone ZoneId zoneId,
            @PathVariable UUID habitId,
            @RequestBody @Valid EditHabitRequest request) {
        EditHabitResponse response = editHabitUseCase.execute(new EditHabitCommand(
                userId,
                zoneId,
                habitId,
                request.title(),
                request.description(),
                request.categoryId(),
                request.difficultyId(),
                request.cycleLength(),
                request.iterationCompleted(),
                request.finished()
        ));
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{habitId}") // TODO: REMOVED ID
    public ResponseEntity<ApiResponse> delete(@PathVariable UUID habitId) {
        deleteHabitUseCase.execute(new DeleteHabitCommand(habitId));
        return ResponseEntity.ok(new ApiResponse("Habit with id: " + habitId + " deleted successfully."));
    }
}
