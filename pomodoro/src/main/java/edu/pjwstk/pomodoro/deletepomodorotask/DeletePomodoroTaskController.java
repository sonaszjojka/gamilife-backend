package edu.pjwstk.pomodoro.deletepomodorotask;

import edu.pjwstk.pomodoro.shared.ApiResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks/pomodoro-tasks")
    public class DeletePomodoroTaskController {

    private final DeletePomodoroTaskUseCase deletePomodoroTaskUseCase;

    public DeletePomodoroTaskController(DeletePomodoroTaskUseCase deletePomodoroTaskUseCase) {
        this.deletePomodoroTaskUseCase = deletePomodoroTaskUseCase;
    }

    @DeleteMapping("/{pomodoroTaskId}")
    public ResponseEntity<ApiResponse> deleteById(@PathVariable("pomodoroTaskId") UUID pomodoroTaskId) {
        deletePomodoroTaskUseCase.execute(pomodoroTaskId);
        return ResponseEntity.status(HttpStatus.OK).body(new ApiResponse("Pomodoro Task with id: " + pomodoroTaskId + " deleted successfully"));
    }
}

