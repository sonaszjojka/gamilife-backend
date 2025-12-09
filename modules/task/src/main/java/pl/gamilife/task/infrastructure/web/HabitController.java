package pl.gamilife.task.infrastructure.web;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.task.application.createhabit.CreateHabitUseCase;
import pl.gamilife.task.application.deletehabit.DeleteHabitUseCase;
import pl.gamilife.task.application.edithabit.EditHabitUseCase;
import pl.gamilife.task.infrastructure.shared.ApiResponse;
import pl.gamilife.task.infrastructure.web.request.CreateHabitRequest;
import pl.gamilife.task.infrastructure.web.request.EditHabitRequest;
import pl.gamilife.task.infrastructure.web.response.CreateHabitResponse;
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
    public ResponseEntity<CreateHabitResponse> create(@PathVariable UUID taskId,
                                                      @RequestBody @Valid CreateHabitRequest request) {
        CreateHabitResponse response = createHabitUseCase.execute(request, taskId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PatchMapping // TODO: CHANGED TO PATCH AND REMOVED ID
    public ResponseEntity<EditHabitResponse> edit(
            @PathVariable UUID taskId,
            @RequestBody @Valid EditHabitRequest request) {
        EditHabitResponse response = editHabitUseCase.execute(request, taskId);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping // TODO: REMOVED ID
    public ResponseEntity<ApiResponse> delete(@PathVariable UUID taskId) {
        deleteHabitUseCase.execute(taskId);
        return ResponseEntity.ok(new ApiResponse("Habit for taskId: " + taskId + " deleted successfully."));
    }
}
