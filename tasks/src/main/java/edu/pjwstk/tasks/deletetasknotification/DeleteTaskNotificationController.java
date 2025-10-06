package edu.pjwstk.tasks.deletetasknotification;

import edu.pjwstk.tasks.shared.ApiResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/{taskId}/task-notifications")
public class DeleteTaskNotificationController {

    private final DeleteTaskNotificationUseCase deleteTaskNotificationUseCase;

    public DeleteTaskNotificationController(DeleteTaskNotificationUseCase deleteTaskNotificationUseCase) {
        this.deleteTaskNotificationUseCase = deleteTaskNotificationUseCase;
    }

    @DeleteMapping("/{taskNotificationId}")
    public ResponseEntity<ApiResponse> deleteById(@PathVariable("taskId") UUID taskId,
                                                  @PathVariable("taskNotificationId") Integer taskNotificationId) {
        deleteTaskNotificationUseCase.execute(taskId, taskNotificationId);
        return ResponseEntity.status(HttpStatus.OK)
                .body(new ApiResponse("Task notification with id: " + taskId + " deleted successfully."));
    }
}