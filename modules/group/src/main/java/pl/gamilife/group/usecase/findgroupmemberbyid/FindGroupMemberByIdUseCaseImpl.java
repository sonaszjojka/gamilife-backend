package pl.gamilife.group.usecase.findgroupmemberbyid;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.group.dto.GroupMemberDto;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupMemberNotFoundException;

@Service
@AllArgsConstructor
public class FindGroupMemberByIdUseCaseImpl implements FindGroupMemberByIdUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;

    @Override
    public GroupMemberDto execute(FindGroupMemberByIdCommand cmd) {
        GroupMember groupMember = groupMemberRepository.findById(cmd.groupMemberId())
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: " +
                        cmd.groupMemberId() + " not found")
                );
        return buildGroupMemberDto(groupMember);
    }

    private GroupMemberDto buildGroupMemberDto(GroupMember groupMember) {
        return GroupMemberDto.builder()
                .groupMemberId(groupMember.getGroupMemberId())
                .memberGroup(new GroupMemberDto.GroupDto(groupMember.getGroupId()))
                .userId(groupMember.getUserId())
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(groupMember.getLeftAt())
                .groupMoney(groupMember.getGroupMoney())
                .totalEarnedMoney(groupMember.getTotalEarnedMoney())
                .build();
    }
}
