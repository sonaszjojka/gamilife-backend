package pl.gamilife.task.controllers;

import pl.gamilife.task.application.createhabit.CreateHabitRequest;
import pl.gamilife.task.application.createhabit.CreateHabitResponse;
import pl.gamilife.task.application.createhabit.CreateHabitUseCase;
import pl.gamilife.task.application.deletehabit.DeleteHabitUseCase;
import pl.gamilife.task.application.edithabit.EditHabitRequest;
import pl.gamilife.task.application.edithabit.EditHabitResponse;
import pl.gamilife.task.application.edithabit.EditHabitUseCase;
import pl.gamilife.task.shared.ApiResponse;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks/{taskId}/habits")
public class HabitController {

    private final CreateHabitUseCase createHabitUseCase;
    private final EditHabitUseCase editHabitUseCase;
    private final DeleteHabitUseCase deleteHabitUseCase;

    public HabitController(CreateHabitUseCase createHabitUseCase,
                           EditHabitUseCase editHabitUseCase,
                           DeleteHabitUseCase deleteHabitUseCase) {
        this.createHabitUseCase = createHabitUseCase;
        this.editHabitUseCase = editHabitUseCase;
        this.deleteHabitUseCase = deleteHabitUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateHabitResponse> create(@PathVariable UUID taskId,
                                                      @RequestBody @Valid CreateHabitRequest request) {
        CreateHabitResponse response = createHabitUseCase.execute(request,taskId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{habitId}")
    public ResponseEntity<EditHabitResponse> edit(
                                                  @PathVariable UUID habitId,
                                                  @PathVariable UUID taskId,
                                                  @RequestBody @Valid EditHabitRequest request) {
        EditHabitResponse response = editHabitUseCase.execute(request, habitId,taskId);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{habitId}")
    public ResponseEntity<ApiResponse> delete(
                                              @PathVariable UUID habitId) {
        deleteHabitUseCase.execute(habitId);
        return ResponseEntity.ok(new ApiResponse("Habit with id: " + habitId + " deleted successfully."));
    }
}
