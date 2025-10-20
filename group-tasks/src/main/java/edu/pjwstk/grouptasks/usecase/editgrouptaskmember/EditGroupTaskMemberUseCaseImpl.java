package edu.pjwstk.grouptasks.usecase.editgrouptaskmember;

import edu.pjwstk.grouptasks.entity.GroupTaskMember;
import edu.pjwstk.grouptasks.exception.GroupTaskMemberNotFoundException;
import edu.pjwstk.grouptasks.repository.GroupTaskMemberRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.util.UUID;
@Service
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
            throw new GroupTaskMemberNotFoundException("Group Task Member with id:" + groupTaskMemberId + " does not exist");
        }
        GroupTaskMember groupTaskMember = groupTaskMemberRepository.findByGroupTaskMemberId(groupTaskMemberId);
        groupTaskMember.setIsMarkedDone(request.isMarkedDone());
        return editGroupTaskMemberMapper.toResponse(groupTaskMemberRepository.save(groupTaskMember));
    }
}
