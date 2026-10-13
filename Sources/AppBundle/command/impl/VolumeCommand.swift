import AppKit
import Common
import ISSoundAdditions

struct VolumeCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        switch args.str("action") ?? "" {
            case "up":
                Sound.output.increaseVolume(by: 0.0625, autoMuteUnmute: true)
            case "down":
                Sound.output.decreaseVolume(by: 0.0625, autoMuteUnmute: true)
            case "mute-toggle":
                Sound.output.isMuted.toggle()
            case "mute-on":
                Sound.output.isMuted = true
            case "mute-off":
                Sound.output.isMuted = false
            case let action: // "set <0-100>"
                if let volume = Int(action.removePrefix("set ")) {
                    Sound.output.setVolume(Float(volume) / 100, autoMuteUnmute: true)
                }
        }
        if args.optBool("gui") != false, let volume = try? Sound.output.readVolume() {
            VolumePanel.shared.update(with: Sound.output.isMuted ? 0 : volume)
        }
        return true
    }
}
