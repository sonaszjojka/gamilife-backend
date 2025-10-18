package edu.pjwstk.tasks.application.createhabit;

import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/habits")
public class CreateHabitController {

    private final CreateHabitUseCase createHabitUseCase;

    public CreateHabitController(CreateHabitUseCase createHabitUseCase) {
        this.createHabitUseCase = createHabitUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateHabitResponse> save(@RequestBody @Valid CreateHabitRequest request) {
        CreateHabitResponse response = createHabitUseCase.execute(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
}
