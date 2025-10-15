package edu.pjwstk.grouptasks.usecase.deletegrouptask;

import edu.pjwstk.grouptasks.repository.GroupTaskRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class DeleteGroupTaskUseCaseImpl implements DeleteGroupTaskUseCase {
    private final GroupTaskRepository groupTaskRepository;
    public DeleteGroupTaskUseCaseImpl(GroupTaskRepository groupTaskRepository) {
        this.groupTaskRepository = groupTaskRepository;
    }


    @Override
    public void execute(UUID groupTaskId) {
       if (!groupTaskRepository.existsByGroupTaskId(groupTaskId))
       {
           throw new IllegalArgumentException("Group Task with id:" + groupTaskId + " does not exist");
       }
         groupTaskRepository.deleteByGroupTaskId(groupTaskId);
    }
}
