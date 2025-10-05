package edu.pjwstk.tasks.deletetask;

import edu.pjwstk.tasks.util.ApiResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks")
public class DeleteTaskController {

    private final DeleteTaskUseCase deleteTaskUseCase;

    public DeleteTaskController(DeleteTaskUseCase deleteTaskUseCase) {
        this.deleteTaskUseCase = deleteTaskUseCase;
    }

    @DeleteMapping("/{taskId}")
    public ResponseEntity<ApiResponse> deleteById(@PathVariable("taskId") UUID taskId) {
        deleteTaskUseCase.execute(taskId);
        return ResponseEntity.status(HttpStatus.OK)
                .body(new ApiResponse("Task with id: " + taskId + " deleted successfully."));
    }
}