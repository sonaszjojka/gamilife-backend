package pl.gamilife.grouptask.usecase.deletegrouptask;

import pl.gamilife.api.task.TasksApi;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.exception.domain.GroupTaskNotFoundException;
import pl.gamilife.grouptask.repository.GroupTaskRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class DeleteGroupTaskUseCaseImpl implements DeleteGroupTaskUseCase {
    private final GroupTaskRepository groupTaskRepository;
    private final TasksApi tasksProvider;
    public DeleteGroupTaskUseCaseImpl(GroupTaskRepository groupTaskRepository, TasksApi tasksProvider) {
        this.groupTaskRepository = groupTaskRepository;
        this.tasksProvider = tasksProvider;
    }


    @Override
    @Transactional
    public void execute(UUID groupTaskId) {
        GroupTask groupTask = groupTaskRepository.findByGroupTaskId(groupTaskId).orElseThrow(
                () -> new GroupTaskNotFoundException("Group Task with id:" + groupTaskId + " does not exist"));

       groupTaskRepository.deleteByGroupTaskId(groupTaskId);
       tasksProvider.deleteTaskByTaskId(groupTask.getTaskId());
    }
}
