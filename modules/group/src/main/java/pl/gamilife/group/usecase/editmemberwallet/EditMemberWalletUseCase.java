package pl.gamilife.group.usecase.editmemberwallet;

import java.util.UUID;

public interface EditMemberWalletUseCase {

    void execute(UUID groupId, UUID memberId, Integer amount);
}
