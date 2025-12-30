package pl.gamilife.group.usecase.grantrewardstomembers;

import org.springframework.stereotype.Service;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupMemberJpaRepository;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

@Service
public class GrantRewardsToMembersUseCaseImpl implements GrantRewardsToMembersUseCase {

    private final GroupMemberJpaRepository groupMemberJpaRepository;

    public GrantRewardsToMembersUseCaseImpl(GroupMemberJpaRepository groupMemberJpaRepository) {
        this.groupMemberJpaRepository = groupMemberJpaRepository;
    }

    @Override
    public Collection<GrantRewardsToMembersResult> execute(GrantRewardsToMembersCommand cmd) {
        if (cmd.groupMemberIds().isEmpty()) {
            return Collections.emptyList();
        }

        List<GroupMember> members = groupMemberJpaRepository.findAllById(cmd.groupMemberIds());
        members.forEach(gm -> gm.gainMoney(cmd.amount()));
        groupMemberJpaRepository.saveAll(members);

        return members.stream()
                .map(gm -> new GrantRewardsToMembersResult(
                        gm.getId(),
                        gm.getGroupMoney(),
                        gm.getTotalEarnedMoney(),
                        gm.getUserId()
                )).toList();
    }
}
