package edu.pjwstk.tasks.edithabit;

import edu.pjwstk.tasks.createhabit.CreateHabitRequest;
import edu.pjwstk.tasks.createhabit.CreateHabitResponse;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/habits")
public class EditHabitController {

    private final EditHabitUseCase editHabitUseCase;

    public EditHabitController(EditHabitUseCase editHabitUseCase) {
        this.editHabitUseCase = editHabitUseCase;
    }

    @PutMapping("/{habitId}")
    public ResponseEntity<EditHabitResponse> save(@RequestBody @Valid EditHabitRequest request,
                                                  @PathVariable("habitId") UUID habitId) {
        EditHabitResponse response = editHabitUseCase.execute(request, habitId);
        return ResponseEntity.ok(response);
    }

}
