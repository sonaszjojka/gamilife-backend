package edu.pjwstk.tasks.createtask;

import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/tasks")
public class CreateTaskController {

    private final CreateTaskUseCase createTaskUseCase;

    public CreateTaskController(CreateTaskUseCase createTaskUseCase) {
        this.createTaskUseCase = createTaskUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateTaskResponse> save(@RequestBody @Valid CreateTaskRequest request) {
        CreateTaskResponse response = createTaskUseCase.execute(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

}
