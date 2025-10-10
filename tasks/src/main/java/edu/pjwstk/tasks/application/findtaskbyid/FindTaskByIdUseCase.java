package edu.pjwstk.tasks.application.findtaskbyid;

import edu.pjwstk.tasks.entity.Task;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.UUID;

@Component
public interface FindTaskByIdUseCase {
    Task execute(UUID taskId);
}
