package pl.gamilife.group.usecase.editmemberwallet;

import org.springframework.stereotype.Service;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;

import java.util.UUID;

@Service
public class EditMemberWalletUseCaseImpl implements EditMemberWalletUseCase {

    private final GroupMemberJpaRepository groupMemberJpaRepository;

    public EditMemberWalletUseCaseImpl(GroupMemberJpaRepository groupMemberJpaRepository) {
        this.groupMemberJpaRepository = groupMemberJpaRepository;
    }

    @Override
    public void execute(UUID groupId, UUID memberId, Integer amount) {
        GroupMember member = groupMemberJpaRepository.findByIdAndGroupId(memberId, groupId).orElseThrow
                (() -> new GroupMemberNotFoundException("Member with Id " + memberId + " does not exist in this group"));

        member.gainMoney(amount);
    }
}
