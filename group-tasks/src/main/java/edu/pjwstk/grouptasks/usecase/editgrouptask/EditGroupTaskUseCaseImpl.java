package edu.pjwstk.grouptasks.usecase.editgrouptask;

import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.repository.GroupTaskRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.UUID;
@Component
public class EditGroupTaskUseCaseImpl implements EditGroupTaskUseCase {
    private final GroupTaskRepository groupTaskRepository;
    private final EditGroupTaskMapper editGroupTaskMapper;

    public EditGroupTaskUseCaseImpl(GroupTaskRepository groupTaskRepository, EditGroupTaskMapper editGroupTaskMapper) {
        this.groupTaskRepository = groupTaskRepository;
        this.editGroupTaskMapper = editGroupTaskMapper;
    }

    @Override
    @Transactional
    public EditGroupTaskResponse execute(UUID groupTaskId, EditGroupTaskRequest req)
    {
        if (!groupTaskRepository.existsByGroupTaskId(groupTaskId))
        {
            throw new IllegalArgumentException("Group Task with id:" + groupTaskId + " does not exist");
        }
        if (req.acceptedDate()!=null&&req.acceptedDate().isBefore(Instant.now()))
        {
            throw new IllegalArgumentException("Accepted date must be in the future");
        }

        GroupTask groupTask=groupTaskRepository.findByGroupTaskId(groupTaskId);
        groupTask.setReward(req.reward());
        groupTask.setIsAccepted(req.isAccepted());
        groupTask.setAcceptedDate(req.acceptedDate());
        groupTask.setDeclineMessage(req.declineMessage());
        groupTask.setAcceptedDate(Instant.now());
        return editGroupTaskMapper.toResponse(groupTaskRepository.save(groupTask));
    }


}
