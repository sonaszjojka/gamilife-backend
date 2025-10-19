package edu.pjwstk.grouptasks.usecase.deletegrouptask;

import edu.pjwstk.common.tasksApi.TasksApi;
import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.repository.GroupTaskRepository;
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
       if (!groupTaskRepository.existsByGroupTaskId(groupTaskId))
       {
           throw new IllegalArgumentException("Group Task with id:" + groupTaskId + " does not exist");
       }

         GroupTask groupTask = groupTaskRepository.findByGroupTaskId(groupTaskId);
         groupTaskRepository.deleteByGroupTaskId(groupTaskId);
        tasksProvider.deleteTaskByTaskId(groupTask.getTaskId());
    }
}
