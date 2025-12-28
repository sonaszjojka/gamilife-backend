package pl.gamilife.grouptask.usecase.deletegrouptask;

import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.exception.domain.GroupTaskNotFoundException;
import pl.gamilife.grouptask.repository.GroupTaskRepository;

import java.util.UUID;

@Service
public class DeleteGroupTaskUseCaseImpl implements DeleteGroupTaskUseCase {
    private final GroupTaskRepository groupTaskRepository;
    private final TaskApi tasksProvider;

    public DeleteGroupTaskUseCaseImpl(GroupTaskRepository groupTaskRepository, TaskApi tasksProvider) {
        this.groupTaskRepository = groupTaskRepository;
        this.tasksProvider = tasksProvider;
    }


    @Transactional
    @Override
    public void execute(UUID userId, UUID groupTaskId) {
        GroupTask groupTask = groupTaskRepository.findByGroupTaskId(groupTaskId).orElseThrow(
                () -> new GroupTaskNotFoundException("Group Task with id:" + groupTaskId + " does not exist"));

        groupTaskRepository.deleteByGroupTaskId(groupTaskId);
        tasksProvider.deleteTaskByTaskId(userId, groupTask.getTaskId());
    }
}
