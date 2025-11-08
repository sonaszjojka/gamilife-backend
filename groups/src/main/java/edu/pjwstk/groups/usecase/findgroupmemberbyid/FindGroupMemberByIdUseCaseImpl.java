package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.api.groups.dto.GroupMemberDto;
import edu.pjwstk.core.exception.common.domain.GroupMemberNotFoundException;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class FindGroupMemberByIdUseCaseImpl implements FindGroupMemberByIdUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupMemberMapper groupMemberMapper;

    public FindGroupMemberByIdUseCaseImpl(GroupMemberJpaRepository groupMemberRepository, GroupMemberMapper groupMemberMapper) {
        this.groupMemberRepository = groupMemberRepository;
        this.groupMemberMapper = groupMemberMapper;
    }

    @Override
    public GroupMemberDto execute(UUID groupMemberId) {
        GroupMember groupMember = groupMemberRepository.findById(groupMemberId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: " + groupMemberId + " not found"));
        return groupMemberMapper.toResponse(groupMember);
    }
}
