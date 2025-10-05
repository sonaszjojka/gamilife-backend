package edu.pjwstk.tasks.createtasknotification;

import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/{taskId}/task-notifications")
public class CreateTaskNotificationController {

    private final CreateTaskNotificationUseCase createTaskNotificationUseCase;

    public CreateTaskNotificationController(CreateTaskNotificationUseCase createTaskNotificationUseCase) {
        this.createTaskNotificationUseCase = createTaskNotificationUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateTaskNotificationResponse> save(@RequestBody @Valid CreateTaskNotificationRequest request,
                                                               @PathVariable(name = "taskId") UUID taskId) {
        CreateTaskNotificationResponse response = createTaskNotificationUseCase.execute(request, taskId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
}
