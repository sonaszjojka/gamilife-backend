package pl.gamilife.group.usecase.findmembersbyidin;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupMemberJpaRepository;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class FindMembersByIdInUseCaseImpl implements FindMembersByIdInUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;

    @Override
    public Collection<FindMembersByIdInResult> execute(FindMembersByIdInCommand cmd) {
        if (cmd.groupMemberIds().isEmpty()) {
            return Collections.emptyList();
        }

        List<GroupMember> members = groupMemberRepository.findAllById(cmd.groupMemberIds());
        return members.stream().map(gm -> new FindMembersByIdInResult(
                gm.getId(),
                gm.getGroupMoney(),
                gm.getTotalEarnedMoney(),
                gm.getUserId()
        )).toList();
    }
}
