package edu.pjwstk.grouptasks.usecase.editgrouptask;

import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.exception.domain.GroupTaskNotFoundException;
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

        GroupTask groupTask=groupTaskRepository.findByGroupTaskId(groupTaskId).orElseThrow(
                ()->new GroupTaskNotFoundException("Group Task with id:" + groupTaskId + " does not exist"));

        Boolean isAccepted=req.isAccepted();
        Instant acceptedDate= null;
        boolean changedToAccepted = Boolean.TRUE.equals(isAccepted)
                && !Boolean.TRUE.equals(groupTask.getIsAccepted());

        if (changedToAccepted)
        {
            acceptedDate= Instant.now();
        }

        groupTask.setReward(req.reward());
        groupTask.setIsAccepted(req.isAccepted());
        groupTask.setAcceptedDate(acceptedDate);
        groupTask.setDeclineMessage(req.declineMessage());

        return editGroupTaskMapper.toResponse(groupTaskRepository.save(groupTask));
    }


}
