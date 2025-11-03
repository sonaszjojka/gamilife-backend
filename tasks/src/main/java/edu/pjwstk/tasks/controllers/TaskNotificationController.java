package edu.pjwstk.tasks.controllers;

import edu.pjwstk.tasks.application.createtasknotification.CreateTaskNotificationRequest;
import edu.pjwstk.tasks.application.createtasknotification.CreateTaskNotificationResponse;
import edu.pjwstk.tasks.application.createtasknotification.CreateTaskNotificationUseCase;
import edu.pjwstk.tasks.application.deletetasknotification.DeleteTaskNotificationUseCase;
import edu.pjwstk.tasks.application.edittasknotification.EditTaskNotificationRequest;
import edu.pjwstk.tasks.application.edittasknotification.EditTaskNotificationResponse;
import edu.pjwstk.tasks.application.edittasknotification.EditTaskNotificationUseCase;
import edu.pjwstk.tasks.shared.ApiResponse;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks/{taskId}/task-notifications")
public class TaskNotificationController {

    private final CreateTaskNotificationUseCase createTaskNotificationUseCase;
    private final EditTaskNotificationUseCase editTaskNotificationUseCase;
    private final DeleteTaskNotificationUseCase deleteTaskNotificationUseCase;

    public TaskNotificationController(CreateTaskNotificationUseCase createTaskNotificationUseCase,
                                      EditTaskNotificationUseCase editTaskNotificationUseCase,
                                      DeleteTaskNotificationUseCase deleteTaskNotificationUseCase) {
        this.createTaskNotificationUseCase = createTaskNotificationUseCase;
        this.editTaskNotificationUseCase = editTaskNotificationUseCase;
        this.deleteTaskNotificationUseCase = deleteTaskNotificationUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateTaskNotificationResponse> create(
            @PathVariable UUID taskId,
            @RequestBody @Valid CreateTaskNotificationRequest request
    ) {
        CreateTaskNotificationResponse response = createTaskNotificationUseCase.execute(request, taskId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{taskNotificationId}")
    public ResponseEntity<EditTaskNotificationResponse> edit(
            @PathVariable UUID taskId,
            @PathVariable Integer taskNotificationId,
            @RequestBody @Valid EditTaskNotificationRequest request
    ) {
        EditTaskNotificationResponse response =
                editTaskNotificationUseCase.execute(request, taskId, taskNotificationId);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{taskNotificationId}")
    public ResponseEntity<ApiResponse> delete(
            @PathVariable UUID taskId,
            @PathVariable Integer taskNotificationId
    ) {
        deleteTaskNotificationUseCase.execute(taskId, taskNotificationId);
        return ResponseEntity.ok(
                new ApiResponse("Task notification with id " + taskNotificationId + " deleted successfully.")
        );
    }
}
