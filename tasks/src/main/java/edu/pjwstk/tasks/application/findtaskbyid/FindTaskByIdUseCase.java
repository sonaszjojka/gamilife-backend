package edu.pjwstk.tasks.application.findtaskbyid;


import edu.pjwstk.api.tasks.dto.TaskDto;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public interface FindTaskByIdUseCase {
    TaskDto execute(UUID taskId);
}
