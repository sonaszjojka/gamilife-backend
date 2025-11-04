package edu.pjwstk.tasks.controllers;

import edu.pjwstk.tasks.application.createhabit.CreateHabitRequest;
import edu.pjwstk.tasks.application.createhabit.CreateHabitResponse;
import edu.pjwstk.tasks.application.createhabit.CreateHabitUseCase;
import edu.pjwstk.tasks.application.deletehabit.DeleteHabitUseCase;
import edu.pjwstk.tasks.application.edithabit.EditHabitRequest;
import edu.pjwstk.tasks.application.edithabit.EditHabitResponse;
import edu.pjwstk.tasks.application.edithabit.EditHabitUseCase;
import edu.pjwstk.tasks.shared.ApiResponse;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/habits")
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
    public ResponseEntity<CreateHabitResponse> create(@RequestBody @Valid CreateHabitRequest request) {
        CreateHabitResponse response = createHabitUseCase.execute(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{habitId}")
    public ResponseEntity<EditHabitResponse> edit(@PathVariable UUID habitId,
                                                  @RequestBody @Valid EditHabitRequest request) {
        EditHabitResponse response = editHabitUseCase.execute(request, habitId);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{habitId}")
    public ResponseEntity<ApiResponse> delete(@PathVariable UUID habitId) {
        deleteHabitUseCase.execute(habitId);
        return ResponseEntity.ok(new ApiResponse("Habit with id: " + habitId + " deleted successfully."));
    }
}
