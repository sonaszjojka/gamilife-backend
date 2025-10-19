package edu.pjwstk.grouptasks.usecase.editgrouptaskmember;

import edu.pjwstk.grouptasks.entity.GroupTaskMember;
import edu.pjwstk.grouptasks.repository.GroupTaskMemberRepository;
import edu.pjwstk.grouptasks.usecase.editgrouptask.EditGroupTaskResponse;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Component;

import java.util.UUID;
@Component
public class EditGroupTaskMemberUseCaseImpl implements EditGroupTaskMemberUseCase {

    private final GroupTaskMemberRepository groupTaskMemberRepository;
    private final EditGroupTaskMemberMapper editGroupTaskMemberMapper;
    public EditGroupTaskMemberUseCaseImpl(GroupTaskMemberRepository groupTaskMemberRepository, EditGroupTaskMemberMapper editGroupTaskMemberMapper) {
        this.groupTaskMemberRepository = groupTaskMemberRepository;
        this.editGroupTaskMemberMapper = editGroupTaskMemberMapper;
    }


    @Override
    @Transactional
    public EditGroupTaskMemberResponse execute(UUID groupTaskMemberId, EditGroupTaskMemberRequest request) {
        if (!groupTaskMemberRepository.existsById(groupTaskMemberId)) {
            throw new IllegalArgumentException("Group Task Member with id:" + groupTaskMemberId + " does not exist");
        }
        GroupTaskMember groupTaskMember = groupTaskMemberRepository.findByGroupTaskMemberId(groupTaskMemberId);
        groupTaskMember.setIsMarkedDone(request.isMarkedDone());
        return editGroupTaskMemberMapper.toResponse(groupTaskMemberRepository.save(groupTaskMember));
    }
}
