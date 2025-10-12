package edu.pjwstk.tasks.application.deletehabit;

import edu.pjwstk.tasks.shared.ApiResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/habits")
public class DeleteHabitController {

    private final DeleteHabitUseCase deleteHabitUseCase;

    public DeleteHabitController(DeleteHabitUseCase deleteHabitUseCase) {
        this.deleteHabitUseCase = deleteHabitUseCase;
    }

    @DeleteMapping("/{habitId}")
    public ResponseEntity<ApiResponse> deleteById(@PathVariable("habitId") UUID habitId) {
        deleteHabitUseCase.execute(habitId);
        return ResponseEntity.status(HttpStatus.OK)
                .body(new ApiResponse("Habit with id: " + habitId + " deleted successfully."));
    }
}