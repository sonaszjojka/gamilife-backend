package edu.pjwstk.grouptasks.usecase.editgrouptask;

import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.exception.GroupTaskNotFoundException;
import edu.pjwstk.grouptasks.repository.GroupTaskRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.UUID;
@Service
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
            throw new GroupTaskNotFoundException("Group Task with id:" + groupTaskId + " does not exist");
        }
        GroupTask groupTask=groupTaskRepository.findByGroupTaskId(groupTaskId);
        boolean isAccepted=req.isAccepted();
        Instant acceptedDate= null;
        if (isAccepted)  acceptedDate=Instant.now();
        groupTask.setReward(req.reward());
        groupTask.setIsAccepted(req.isAccepted());
        groupTask.setAcceptedDate(acceptedDate);
        groupTask.setDeclineMessage(req.declineMessage());
        groupTask.setLastEdit(Instant.now());
        return editGroupTaskMapper.toResponse(groupTaskRepository.save(groupTask));
    }


}
