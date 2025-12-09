package pl.gamilife.task.infrastructure.web;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
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

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/tasks/{taskId}/habit") // TODO: REMOVED 's'
public class HabitController {

    private final CreateHabitUseCase createHabitUseCase;
    private final EditHabitUseCase editHabitUseCase;
    private final DeleteHabitUseCase deleteHabitUseCase;

    @PostMapping
    public ResponseEntity<CreateHabitResult> create(@PathVariable UUID taskId,
                                                    @RequestBody @Valid CreateHabitRequest request) {
        CreateHabitResult response = createHabitUseCase.execute(new CreateHabitCommand(
                taskId,
                request.cycleLength()
        ));
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PatchMapping // TODO: CHANGED TO PATCH AND REMOVED ID
    public ResponseEntity<EditHabitResponse> edit(
            @PathVariable UUID taskId,
            @RequestBody @Valid EditHabitRequest request) {
        EditHabitResponse response = editHabitUseCase.execute(new EditHabitCommand(taskId, request.cycleLength(), request.finished()));
        return ResponseEntity.ok(response);
    }

    @DeleteMapping // TODO: REMOVED ID
    public ResponseEntity<ApiResponse> delete(@PathVariable UUID taskId) {
        deleteHabitUseCase.execute(new DeleteHabitCommand(taskId));
        return ResponseEntity.ok(new ApiResponse("Habit for taskId: " + taskId + " deleted successfully."));
    }
}
