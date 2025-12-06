package pl.gamilife.gamification.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;
import pl.gamilife.gamification.domain.exception.UserDoesNotHaveEnoughItemsException;

import java.util.Objects;
import java.util.UUID;

@Getter
@Entity
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString(exclude = {"item"})
@Table(name = "user_inventory_item")
public class UserInventoryItem {
    @Id
    @Builder.Default
    @Column(name = "id", nullable = false)
    private UUID id = UUID.randomUUID();

    @Column(name = "item_id", nullable = false, insertable = false, updatable = false)
    private UUID itemId;

    @NotNull
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "item_id", nullable = false)
    private Item item;

    @NotNull
    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @NotNull
    @Column(name = "quantity", nullable = false)
    private Integer quantity;

    @NotNull
    @Column(name = "is_equipped", nullable = false)
    @Builder.Default
    private Boolean isEquipped = false;

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        UserInventoryItem that = (UserInventoryItem) o;
        return Objects.equals(id, that.id);
    }

    public boolean doesBelongTo(UUID userId) {
        return this.userId.equals(userId);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }

    public void incrementQuantityBy(int amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Quantity increment be greater than 0");
        }

        this.quantity += amount;
    }

    public int quickSellItems(Integer amount) {
        if (amount > quantity) {
            throw new UserDoesNotHaveEnoughItemsException(
                    String.format("User has only %s such items in inventory", quantity)
            );
        }

        quantity -= amount;

        return item.getQuickSellValue() * amount;
    }

    public boolean changeEquippedStatus(boolean newStatus) {
        if (isEquipped == newStatus) {
            return false;
        }

        this.isEquipped = newStatus;
        return true;
    }
}