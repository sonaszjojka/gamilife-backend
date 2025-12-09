package pl.gamilife.task.infrastructure.web;

import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.task.application.createtasknotification.CreateTaskNotificationUseCase;
import pl.gamilife.task.application.deletetasknotification.DeleteTaskNotificationUseCase;
import pl.gamilife.task.application.edittasknotification.EditTaskNotificationUseCase;
import pl.gamilife.task.infrastructure.shared.ApiResponse;
import pl.gamilife.task.infrastructure.web.request.CreateTaskNotificationRequest;
import pl.gamilife.task.infrastructure.web.request.EditTaskNotificationRequest;
import pl.gamilife.task.infrastructure.web.response.CreateTaskNotificationResponse;
import pl.gamilife.task.infrastructure.web.response.EditTaskNotificationResponse;

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

    @PatchMapping("/{taskNotificationId}") // TODO: CHANGE TO PATCH
    public ResponseEntity<EditTaskNotificationResponse> edit(
            @PathVariable UUID taskId,
            @PathVariable UUID taskNotificationId,
            @RequestBody @Valid EditTaskNotificationRequest request
    ) {
        EditTaskNotificationResponse response =
                editTaskNotificationUseCase.execute(request, taskId, taskNotificationId);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{taskNotificationId}")
    public ResponseEntity<ApiResponse> delete(
            @PathVariable UUID taskId,
            @PathVariable UUID taskNotificationId
    ) {
        deleteTaskNotificationUseCase.execute(taskId, taskNotificationId);
        return ResponseEntity.ok(
                new ApiResponse("Task notification with id " + taskNotificationId + " deleted successfully.")
        );
    }
}
