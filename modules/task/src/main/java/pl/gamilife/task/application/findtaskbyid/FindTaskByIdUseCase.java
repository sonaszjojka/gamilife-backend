package pl.gamilife.task.application.findtaskbyid;


import org.springframework.stereotype.Component;
import pl.gamilife.api.task.dto.TaskDto;

import java.util.UUID;

@Component
public interface FindTaskByIdUseCase {
    TaskDto execute(UUID taskId);
}
