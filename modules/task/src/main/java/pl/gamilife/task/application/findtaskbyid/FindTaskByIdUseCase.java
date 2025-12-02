package pl.gamilife.task.application.findtaskbyid;


import pl.gamilife.api.task.dto.TaskDto;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public interface FindTaskByIdUseCase {
    TaskDto execute(UUID taskId);
}
